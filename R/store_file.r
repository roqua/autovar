# store_file

# stores a file to disk
# file name is a name you give to it
store_file <- function(filename,file_type = c('SPSS','STATA')) {
  if (missing(filename)) {
    filename <- av_state$real_file_name
  }
  if (missing(file_type)) {
    file_type <- 'SPSS'
  }
  filename <- basename(filename)
  working_dir <- paste(gsub('\\\\','/',system('pwd',intern=TRUE)),'/',sep="")
  tarfiles <- ""
  for (name in names(av_state$data)) {
    data_frame <- av_state$data[[name]]
    datafile<-paste(filename,"_",name,".txt",sep="")
    codefile<-paste(filename,"_",name,".sps",sep="")
    tarfiles <- paste(tarfiles,datafile,codefile)
    switch(file_type,
      SPSS = writeMyForeignSPSS(data_frame,paste(working_dir,datafile,sep=""),paste(working_dir,codefile,sep="")),
      STATA = store_file_stata(data_frame,paste(working_dir,datafile,sep=""),paste(working_dir,codefile,sep=""))
    )
  }
  tarcmd <- paste("tar -cvvf ",filename,".tar ",tarfiles,sep="")
  #print(tarcmd)
  system(tarcmd,intern=TRUE)
  if (!interactive()) {
    #system(paste("rm",tarfiles),intern=TRUE)
  }
  cat("store_file: created",paste(working_dir,filename,".tar",sep=""),"\n")
}

store_file_stata <- function(...) {
  cat("store_file_stata: not implemented\n")
}

adQuote <- function (x) { paste("\"", x, "\"", sep = "") }
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) { is.na(x) | abs(x - round(x)) < tol }
adF8 <- function (x,dfn) { as.character(lapply(x,function(y) if (length(grep("datum",y)) < 1) paste(y,ifelse(all(is.wholenumber(dfn[[y]])),'(F8)','(F8.2)')) else paste(y,'(DATE11)'))) }

writeMyForeignSPSS <- function (df, datafile, codefile, varnames = NULL) {
    dfn <- lapply(df, function(x) if (is.factor(x))
        as.numeric(x)-1
    else x)
    write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE,
        sep = ",", quote = FALSE, na = "", eol = ",\n")
    varlabels <- names(df)
    if (is.null(varnames)) {
        #varnames <- abbreviate(names(df), 8L)
        varnames <- varlabels
        #if (any(sapply(varnames, nchar) > 8L))
        #    stop("I cannot abbreviate the variable names to eight or fewer letters")
        if (any(varnames != varlabels))
            warning("some variable names were abbreviated")
    }
    varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
    dl.varnames <- varnames
    if (any(chv <- sapply(df, is.character))) {
        lengths <- sapply(df[chv], function(v) max(nchar(v)))
        if (any(lengths > 255L))
            stop("Cannot handle character variables longer than 255")
        lengths <- paste("(A", lengths, ")", sep = "")
        star <- ifelse(c(FALSE, diff(which(chv) > 1L)), " *",
            " ")
        dl.varnames[chv] <- paste(star, dl.varnames[chv], lengths)
    }
    cat("SET DECIMAL DOT.\n\n", file = codefile)
    cat("DATA LIST FILE=", adQuote(datafile), " free (\",\")\n",
        file = codefile, append = TRUE)
    cat("/", dl.varnames, " .\n\n", file = codefile, append = TRUE)
    cat("FORMATS", adF8(dl.varnames,dfn), " .\n\n", file = codefile, append = TRUE)
    cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
    cat(paste(varnames, adQuote(varlabels), "\n"), ".\n", file = codefile,
        append = TRUE)
    factors <- sapply(df, is.factor)
    if (any(factors)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        for (v in which(factors)) {
            cat("/\n", file = codefile, append = TRUE)
            cat(varnames[v], " \n", file = codefile, append = TRUE)
            levs <- levels(df[[v]])
            cat(paste(seq_along(levs)-1, adQuote(levs), "\n", sep = " "),
                file = codefile, append = TRUE)
        }
        cat(".\n", file = codefile, append = TRUE)
    }
    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}
