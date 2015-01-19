#' Export a modified data set as an SPSS readable .sas file
#' 
#' This function exports all subsets in the specified \code{av_state} to individual output files. All output files are subsequently packed in a .tar file. Data sets with less than 80 columns are stored inline in the .sas script, otherwise, the data is stored in separate files.
#' @param av_state an object of class \code{av_state}
#' @param filename the name that the exported files should be saved under. Do not include a path. When this argument is missing, the filename of the input file (used in \code{\link{load_file}}) is substituted.
#' @param inline_data boolean argument to determine whether data should be stored inline in the .sas script. This argument is optional, and defaults to \code{TRUE} if the data set has less than 80 columns, and to \code{FALSE} otherwise.
#' @param file_type sets the type of file export that should be used. Currently, only \code{'SPSS'} is supported.
#' @examples
#' av_state <- load_file("../data/input/ID68 basisbestand.sav",log_level=3)
#' av_state <- add_trend(av_state)
#' av_state <- set_timestamps(av_state,date_of_first_measurement="2012-07-12",
#'                            measurements_per_day=3,log_level=3)
#' store_file(av_state)
#' @export
store_file <- function(av_state,filename,inline_data,file_type = c('SPSS','STATA')) {
  assert_av_state(av_state)
  if (missing(filename)) {
    filename <- av_state$real_file_name
  }
  filename <- basename(filename)
  if (missing(inline_data)) {
    # inline data only works if there are less than 80 columns
    inline_data <- length(av_state$data[[1]]) <= 80
  }
  if (missing(file_type)) {
    file_type <- 'SPSS'
  }
  working_dir <- paste(getwd(),'/',sep="")
  store_func <- NULL
  if (inline_data) {
    store_func <- store_file_inline
  } else {
    store_func <- store_file_separate
  }
  tarfiles <- store_func(working_dir,filename,file_type,av_state)
  
  tarcmd <- paste("tar -cvvf \"",filename,".tar\" ",tarfiles,sep="")
  system(tarcmd,intern=TRUE)

  scat(av_state$log_level,2,"store_file: created",paste(working_dir,filename,".tar",sep=""),"\n")
}

store_file_separate <- function(working_dir,filename,file_type,av_state) {
  tarfiles <- ""
  for (name in names(av_state$data)) {
    data_frame <- av_state$data[[name]]
    datafile<-paste(filename,"_",name,".txt",sep="")
    codefile<-paste(filename,"_",name,".sps",sep="")
    tarfiles <- paste(tarfiles,adQuote(datafile),adQuote(codefile))
    switch(file_type,
      SPSS = writeMyForeignSPSS(df = data_frame,datafile = paste(working_dir,datafile,sep=""),codefile = paste(working_dir,codefile,sep="")),
      STATA = store_file_stata_separate(df = data_frame,datafile = paste(working_dir,datafile,sep=""),codefile = paste(working_dir,codefile,sep=""))
    )
  }
  tarfiles
}

store_file_inline <- function(working_dir,filename,file_type,av_state) {
  tarfiles <- ""
  for (name in names(av_state$data)) {
    data_frame <- av_state$data[[name]]
    codefile<-paste(filename,"_",name,".sps",sep="")
    tarfiles <- paste(tarfiles,adQuote(codefile))
    switch(file_type,
      SPSS = writeMyForeignSPSS_inline(df = data_frame,codefile = paste(working_dir,codefile,sep="")),
      STATA = store_file_stata_inline(df = data_frame,codefile = paste(working_dir,codefile,sep=""))
    )
  }
  tarfiles
}

store_file_stata_inline <- function(...) {
  cat("store_file_stata_inline: not implemented\n")
}

store_file_stata_separate <- function(...) {
  cat("store_file_stata_separate: not implemented\n")
}

adQuote <- function (x) { paste("\"", x, "\"", sep = "") }
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) { is.na(x) | abs(x - round(x)) < tol }
determineLevel <- function(data_column) {
  determine_function <- switch(class(data_column),
    numeric=determineLevelNumeric,
    integer=determineLevelNumeric,
    factor=determineLevelFactor
  )
  if (is.null(determine_function)) determine_function <- determineLevelCharacter
  determine_function(data_column)
}
determineLevelNumeric <- function(data_column) {
  '(SCALE)'
}
determineLevelFactor <- function(data_column) {
  if (length(levels(data_column)) > 2) {
    '(ORDINAL)'
  } else {
    '(NOMINAL)'
  }
}
determineLevelCharacter <- function(data_column) {
  '(NOMINAL)'
}
determineFormat <- function(data_column) {
  determine_function <- switch(class(data_column),
    numeric=determineFormatNumeric,
    integer=determineFormatNumeric,
    factor=determineFormatFactor
  )
  if (is.null(determine_function)) determine_function <- determineFormatCharacter
  determine_function(data_column)
}
determineFormatNumeric <- function(data_column) {
  if (all(is.wholenumber(data_column))) {
    '(F8)'
  } else {
    '(F8.2)'
  }
}
determineFormatCharacter <- function(data_column) {
  length <- max(1,max(nchar(as.character(data_column))))
  paste("(A", length, ")", sep = "")
}
determineFormatFactor <- function(data_column) {
  '(F8)'
}
adF8 <- function(x,dfn) { 
  as.character(lapply(x,function(y) paste(y,determineFormat(dfn[[y]]))))
}

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
    if (any(chv <- sapply(df, is_unknown))) {
        lengths <- sapply(df[chv], function(v) max(nchar(as.character(v))))
        if (any(lengths > 255L))
            stop("Cannot handle character variables longer than 255")
        lengths <- paste("(A", lengths, ")", sep = "")
        #star <- ifelse(c(FALSE, diff(which(chv) > 1L)), " *", " ")
        dl.varnames[chv] <- paste(dl.varnames[chv], lengths)
    }
    cat("SET DECIMAL DOT.\n\n", file = codefile)
    cat("DATA LIST FILE=", adQuote(datafile), " free (\",\")\n",
        file = codefile, append = TRUE)
    cat("/", dl.varnames, " .\n\n", file = codefile, append = TRUE)
    cat("FORMATS", adF8(varnames,df), " .\n\n", file = codefile, append = TRUE)
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
    cat("\nVARIABLE LEVEL\n", file = codefile, append = TRUE)
    for (v in varnames) {
      cat(" /", v, determineLevel(df[[v]]),"\n",file = codefile, append = TRUE)
    }
    cat(".\n", file = codefile, append = TRUE)
    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}
is_unknown <- function(x) {
  class(x) != "numeric" && class(x) != "integer" && class(x) != "factor"
}
writeMyForeignSPSS_inline <- function (df, codefile, varnames = NULL) {
    dfn <- lapply(df, function(x) if (is.factor(x))
        as.numeric(x)-1
    else x)
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
    if (any(chv <- sapply(df, is_unknown))) {
        lengths <- sapply(df[chv], function(v) max(nchar(as.character(v))))
        if (any(lengths > 255L))
            stop("Cannot handle character variables longer than 255")
        lengths <- paste("(A", lengths, ")", sep = "")
        #star <- ifelse(c(FALSE, diff(which(chv) > 1L)), " *", " ")
        dl.varnames[chv] <- paste(dl.varnames[chv], lengths)
    }
    cat("SET DECIMAL DOT.\n\n", file = codefile)
    cat("DATA LIST free (\",\")\n",
        file = codefile, append = TRUE)
    cat("/", dl.varnames, " .\n\n", file = codefile, append = TRUE)
    cat("BEGIN DATA\n", file = codefile, append = TRUE)
    write.table(dfn, file = codefile, row.names = FALSE, col.names = FALSE,
                sep = ",", append = TRUE, quote = FALSE, na = "", eol = ",\n")
    cat("END DATA.\n\n", file = codefile, append = TRUE)
    cat("FORMATS", adF8(varnames,df), " .\n\n", file = codefile, append = TRUE)
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
    cat("\nVARIABLE LEVEL\n", file = codefile, append = TRUE)
    for (v in varnames) {
      cat(" /", v, determineLevel(df[[v]]),"\n",file = codefile, append = TRUE)
    }
    cat(".\n", file = codefile, append = TRUE)
    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}
