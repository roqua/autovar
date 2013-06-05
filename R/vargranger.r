# Granger causality
vargranger <- function(varest,log_level=0) {
  scat(log_level,2,"\nGranger causality Wald tests\n")
  res <- vargranger_call(varest)
  sprint(log_level,1,res)
  tos <- vargranger_to_string(varest,res)
  if (tos != '') {
    scat(log_level,2,'Vargranger causes: ',tos,'\n',sep='')
  } else {
    scat(log_level,2,'No significant Granger causes detected.\n')
  }
  tos
}

# Print Granger Statistics
print_granger_statistics <- function(av_state) {
  lst <- c(av_state$accepted_models,av_state$rejected_models)
  #print_vargranger_list(av_state,lst,"processed models")
  lst2 <- filter_lag_zero_models(lst[find_models(lst,list(restrict=FALSE))])
  print_vargranger_list(av_state,lst2,"unrestricted models")
  print_vargranger_list(av_state,av_state$accepted_models,"valid models")
  lst2 <- filter_lag_zero_models(av_state$accepted_models[find_models(av_state$accepted_models,list(restrict=FALSE))])
  print_vargranger_list(av_state,lst2,"valid unrestricted models")
}

filter_lag_zero_models <- function(lst) {
  lst2 <- NULL
  for (model in lst) {
    if (is.null(model$varest$restrictions)) {
      lst2 <- c(lst2,list(model))
    }
  }
  lst2
}

vargranger_graph <- function(av_state) {
  vargranger_graph_aux(av_state,av_state$accepted_models)
}

vargranger_graph_aux <- function(av_state,lst) {
  str <- NULL
  rls <- list()
  rlsa <- list()
  rlsr <- list()
  nonecount <- 0
  for (model in lst) {
    res <- vargranger_call(model$varest)
    foundflag <- FALSE
    for (row in df_in_rows(res)) {
      if (row$P <= av_state_significance(model$varest)) {
        weight <- 2
        foundflag <- TRUE
      } else if (row$P <= 2*av_state_significance(model$varest)) {
        weight <- 1
        foundflag <- TRUE
      } else { 
        next
      }
      edgename <- paste(unprefix_ln(row$Excluded),unprefix_ln(row$Equation))
      if (is.null(rls[[edgename]])) {
        rls[[edgename]] <- 0
      }
      rls[[edgename]] <- rls[[edgename]]+weight
      if (weight == 2) {
        if (is.null(rlsr[[edgename]])) {
          rlsr[[edgename]] <- 0
        }
        rlsr[[edgename]] <- rlsr[[edgename]]+1
      } else {
        if (is.null(rlsa[[edgename]])) {
          rlsa[[edgename]] <- 0
        }
        rlsa[[edgename]] <- rlsa[[edgename]]+1
      }
    }
    if (!foundflag) {
      nonecount <- nonecount+1
    }
  }
  i <- 0
  enms <- names(rls)
  edgelabels <- NULL
  for (edgeweight in rls) {
    i <- i+1
    edgename <- enms[[i]]
    edgelabel <- ''
    if (!is.null(rlsr[[edgename]])) { 
      edgelabel <- paste(edgelabel,
                         rlsr[[edgename]],
                         ' model',
                         ifelse(rlsr[[edgename]] == 1,'','s'),
                         sep='')
    }
    if (!is.null(rlsa[[edgename]])) {
      edgelabel <- paste(edgelabel,
                         ifelse(edgelabel == '','(','\n(+'),
                         rlsa[[edgename]],
                         ' almost)',
                         sep='')
    }
    edgelabels <- c(edgelabels,edgelabel)
    str <- paste(str,edgename,' ',edgeweight,'\n',sep='')
  }
  if (!is.null(str)) {
    list(str=str,nonecount=nonecount,
         allcount=length(lst),edgelabels=edgelabels)
  } else {
    NULL
  }
}

vargranger_plot <- function(av_state) {
  graphi <- vargranger_graph(av_state)
  if (!is.null(graphi)) {
    graphstring <- graphi$str
    # TODO: check if temp files are cleaned up
    file <- tempfile()
    #cat("tempfile:",file,"\n")
    cat(graphstring, file = file)
    a <- read.graph(file,format="ncol",directed=TRUE,weights="yes")
    cols <- c('springgreen4','steelblue','chocolate1')
    E(a)$width <- E(a)$weight
    E(a)$label <- graphi$edgelabels
    plot(a,
         edge.arrow.size=2,
         edge.arrow.width=2,
         edge.color='gray15',
         edge.curved=TRUE,
         edge.label.family='sans',
         edge.label.color=colors()[[190]],
         edge.label.cex=0.75,
         vertex.size=65,
         vertex.label.family='sans',
         vertex.label.cex=1,
         vertex.color=cols[1:(length(V(a)))],
         vertex.label.color='black',
         vertex.label.font=2,
         main="Granger causality",
         sub=paste('found in',graphi$allcount - graphi$nonecount,'out of',graphi$allcount,'valid models'))
    gname <- gsub("\\.[^ ]{3,4}$","",basename(av_state$real_file_name))
    fname <- gname
    i <- 0
    while (file.exists(paste(fname,'.pdf',sep=''))) {
      i <- i + 1
      fname <- paste(gname,'_',i,sep='')
    }
    fname <- paste(fname,'.pdf',sep='')
    dev.copy2pdf(file=fname)
    scat(av_state$log_level,3,"\nGranger causality plot saved to \"",fname,"\"\n",sep='')
    invisible(a)
  }
}

iplot_test <- function(a,...) {
  plot(a,
       edge.arrow.size=2,
       edge.arrow.width=2,
       edge.color='gray15',
       edge.curved=TRUE,
       edge.label.family='sans',
       edge.label.color=colors()[[190]],
       edge.label.cex=0.75,
       vertex.size=65,
       vertex.label.family='sans',
       vertex.label.cex=1,
       vertex.color=cols[1:(length(V(a)))],
       vertex.label.color='black',
       vertex.label.font=2,
       main="Granger causality",
       sub=paste('found in X out of Y valid models'),...)
}

df_in_rows <- function(df) {
  lst <- NULL
  if (!is.null(df)) {
    for (i in 1:(dim(df)[[1]])) {
      lst <- c(lst,list(df[i,]))
    }
  }
  lst
}

vargranger_call <- function(varest) {
  if (av_state_small(varest)) {
    vargranger_aux_small(varest)
  } else {
    vargranger_aux(varest)
  }
}

vargranger_aux <- function(varest) {
  res <- NULL
  for (eqname in dimnames(varest$y)[[2]]) {
    for (exname in dimnames(varest$y)[[2]]) {
      if (exname == eqname) { next }
      gres <- granger_causality(varest,exname,eqname)
      gsign <- granger_causality_sign(varest,exname,eqname)
      if (is.null(gres)) { next }
      df <- get_named(gres$parameter,'df1')
      chi2 <- gres$statistic[1,1]*df
      P <- chi_squared_prob(chi2,df)
      if (is.null(res)) {
        res <- data.frame(Equation=eqname,
                          Excluded=exname,
                          sign=gsign,
                          chi2=chi2,df=df,
                          P=P,
                          stringsAsFactors=FALSE)
      } else {
        res <- rbind(res,list(eqname,exname,gsign,chi2,df,P))
      }
    }
  }
  res
}

vargranger_aux_small <- function(varest) {
  res <- NULL
  for (eqname in dimnames(varest$y)[[2]]) {
    for (exname in dimnames(varest$y)[[2]]) {
      if (exname == eqname) { next }
      gres <- granger_causality(varest,exname,eqname)
      gsign <- granger_causality_sign(varest,exname,eqname)
      if (is.null(gres)) { next }
      F <- gres$statistic[1,1]
      df <- get_named(gres$parameter,'df1')
      #df_r <- get_named(gres$parameter,'df2')
      df_r <- vargranger_df_r(varest)
      #P <- gres$p.value[1,1]
      P <- pf(F,df,df_r,lower.tail=FALSE)
      if (is.null(res)) {
        res <- data.frame(Equation=eqname,
                          Excluded=exname,
                          sign=gsign,
                          F=F,df=df,df_r=df_r,
                          P=P,
                          stringsAsFactors=FALSE)
      } else {
        res <- rbind(res,list(eqname,exname,gsign,F,df,df_r,P))
      }
    }
  }
  res
}

granger_causality <- function(varest,cause,equation) {
  varest <- process_restricted_varest(varest,cause,equation)
  gres <- NULL
  suppressWarnings(tryCatch(gres <- causality2(varest,cause=cause,equation=equation)$Granger,error=function(e) { }))
  gres
}

process_restricted_varest <- function(varest, cause, equation) {
  if (!is.null(varest$restrictions)) {
    orestricts <- varest$restrictions[equation,]
    varest$p <- sum(orestricts[names(orestricts) %in% get_lag_varnames(varest,cause)])
    excluded_names <- names(orestricts)[which(orestricts == 0)]
    # 0 means exclude
    varest$datamat <- varest$datamat[, !(colnames(varest$datamat) %in% excluded_names)]
  }
  varest
}

get_lag_varnames <- function(varest,varname) {
  len <- length(colnames(varest$datamat))
  lst <- NULL
  for (i in 1:len) {
    lst <- c(lst,paste(varname,'.l',i,sep=''))
  }
  lst
}

other_varname <- function(varest,cause) {
  vars <- restriction_matrix_rownames(varest)
  vars[which(vars != cause)]
}

vargranger_df_r <- function(varest) {
  varest$obs-nr_pars_estimated_average(varest)
}

get_named <- function(arr,name) {
  if (name %in% names(arr)) {
    arr[[which(name == names(arr))]]
  } else {
    NULL
  }
}

granger_causality_sign <- function(varest,exname,eqname) {
  coefs <- varest$varresult[[eqname]]$coefficients
  dnames <- names(coefs)
  vars <- !is.na(str_locate(dnames,paste(exname,"\\.l[0-9]+$",sep=''))[,1])
  matching_indices <- which(vars)
  sum(coefs[matching_indices])
}

vargranger_to_string <- function(varest,res,include_significance=TRUE) {
  # res is a vargranger_aux result
  str <- NULL
  for (row in df_in_rows(res)) {
    if (row$P <= av_state_significance(varest)) {
      str <- c(str,paste(unprefix_ln(row$Excluded),
                         ' Granger causes ',
                         unprefix_ln(row$Equation),
                         ifelse(include_significance,
                                paste(' (',signif(row$P,digits=3),')',sep=''),
                                ''),sep=''))
    } else if (row$P <= 2*av_state_significance(varest)) {
      str <- c(str,paste(unprefix_ln(row$Excluded),
                         ' almost Granger causes ',
                         unprefix_ln(row$Equation),
                         ifelse(include_significance,
                                paste(' (',signif(row$P,digits=3),')',sep=''),
                                ''),sep=''))
    }
  }
  if (!is.null(str)) {
    str <- paste(str,collapse='; ')
  } else {
    str <- ''
  }
  str
}

print_vargranger_list <- function(av_state,lst,title) {
  if (length(lst) != 0) {
    scat(av_state$log_level,3,
         paste("\nGranger causality summary of all ",
               length(lst)," ",title,":\n",sep=''))
    glist <- vargranger_list(lst)
    for (i in 1:nr_rows(glist)) {
      gres <- glist[i,]
      scat(av_state$log_level,3,"  ",gres$desc,"\n",sep='')
    }
  }
}

vargranger_list <- function(lst) {
  llst <- list()
  for (item in lst) {
    varest <- item$varest
    res <- vargranger_call(varest)
    noneflag <- TRUE
    for (row in df_in_rows(res)) {
      if (row$P <= 2*av_state_significance(varest)) {
        noneflag <- FALSE
        gsign <- granger_causality_sign(varest,row$Excluded,row$Equation)
        causevr <- unprefix_ln(row$Excluded)
        othervr <- unprefix_ln(row$Equation)
        cmbvr <- paste(causevr,'.',othervr,sep='')
        if (is.null(llst[[cmbvr]])) {
          llst[[cmbvr]] <- list(causevr=causevr,
                                othervr=othervr,
                                signplus=0,
                                signminus=0,
                                cnt=0,
                                cnta=0)
        }
        if (gsign > 0) {
          llst[[cmbvr]]$signplus <- llst[[cmbvr]]$signplus + 1
        } else if (gsign < 0) {
          llst[[cmbvr]]$signminus <- llst[[cmbvr]]$signminus + 1
        }
        if (row$P <= av_state_significance(varest)) {
          llst[[cmbvr]]$cnt <- llst[[cmbvr]]$cnt +1
        } else {
          llst[[cmbvr]]$cnta <- llst[[cmbvr]]$cnta +1
        }
      }
    }
    if (noneflag) {
      if (is.null(llst[["none"]])) {
        llst[["none"]] <- list(causevr='',
                               othervr='',
                               signplus=0,
                               signminus=0,
                               cnt=0,
                               cnta=0)
      }
      llst$none$cnt <- llst$none$cnt +1
    }
  }
  causevr <- NULL
  othervr <- NULL
  signplus <- NULL
  signminus <- NULL
  cnt <- NULL
  cnta <- NULL
  for (item in llst) {
    causevr <- c(causevr,item$causevr)
    othervr <- c(othervr,item$othervr)
    signplus <- c(signplus,item$signplus)
    signminus <- c(signminus,item$signminus)
    cnt <- c(cnt,item$cnt)
    cnta <- c(cnta,item$cnta)
  }
  df <- data.frame(causevr=causevr,othervr=othervr,signplus=signplus,
                   signminus=signminus,cnt=cnt,cnta=cnta,stringsAsFactors=FALSE)
  df <- cbind(df,list(sign=signplus-signminus))
  df <- cbind(df,list(tcnt=cnt+cnta))
  df <- cbind(df,list(perc=(cnt+cnta)/length(lst)))
  df <- df[with(df,order(df$tcnt,df$cnt,df$causevr,decreasing=TRUE)),]
  varwidth <- max(sapply(unique(c(df$causevr,df$othervr)),nchar))
  df <- cbind(df,
              list(desc=sapply(df_in_rows(df),
                               function(x) compact_varline(x,varwidth))),
              stringsAsFactors=FALSE)
  df
}

compact_varline <- function(x,varwidth) {
  str <- NULL
  percstr <- format_as_percentage(x$perc)
  causevr <- format(x$causevr,width=varwidth,justify="left")
  othervr <- format(x$othervr,width=varwidth,justify="left")
  if (x$causevr == '') {
    str <- paste(percstr,'   ',
                 format('<None>',width=varwidth,justify="left"),
                 paste(rep(' ',18+varwidth),collapse=''),
                 '   (',
                 x$cnt,' model',
                 ifelse(x$cnt == 1,'','s'),
                 ')',sep='')
  } else {
    sign <- ifelse(x$sign > 0,'+',ifelse(x$sign < 0,'-',' '))
    str <- paste(percstr,'   ',causevr,' ',sign,'Granger causes',sign,' ',
                 othervr,'   (',sep='')
    if (x$cnt > 0) {
      str <- paste(str,x$cnt,' model',
                   ifelse(x$cnt == 1,'','s'),sep='')
      if (x$cnta > 0) {
        str <- paste(str,' +',sep='')
      } else {
        str <- paste(str,')',sep='')
      }
    }
    if (x$cnta > 0) {
      str <- paste(str,x$cnta,' almost)',sep='')
    }
    if (x$signplus != 0 || x$signminus != 0) {
      str <- paste(str,' (sign: ',sep='')
      if (x$signplus != 0) {
        str <- paste(str,x$signplus,' +',sep='')
        if (x$signminus != 0) {
          str <- paste(str,', ',sep='')
        }
      }
      if (x$signminus != 0) {
        str <- paste(str,x$signminus,' -',sep='')
      }
      str <- paste(str,')',sep='')
    }
  }
  str
}

vargranger_line <- function(varest,...) {
  vargranger_to_string(varest,vargranger_call(varest),...)
}

format_as_percentage <- function(frac) {
  paste(format(round(100*frac,digits=2),nsmall=2,width=6),"%",sep='')
}


# causality from VARS package with improvements to support >2 variables
causality2 <- function (x, cause = NULL, equation = NULL, vcov. = NULL, boot = FALSE, boot.runs = 100) {
  if (!(class(x) == "varest")) {
    stop("\nPlease provide an object of class 'varest', generated by 'var()'.\n")
  }
  K <- x$K
  p <- x$p
  obs <- x$obs
  type <- x$type
  obj.name <- deparse(substitute(x))
  y <- x$y
  y.names <- colnames(x$y)
  if (is.null(cause)) {
    cause <- y.names[1]
    warning("\nArgument 'cause' has not been specified;\nusing first variable in 'x$y' (", 
            cause, ") as cause variable.\n")
  }
  else {
    if (!all(cause %in% y.names)) 
      stop("Argument cause does not match variables names.\n")
  }
  y1.names <- subset(y.names, subset = y.names %in% cause)
  y2.names <- subset(y.names, subset = y.names %in% equation)
  Z <- x$datamat[, -c(1:K)]
  xMlm <- vars:::toMlm(x)
  PI <- coef(xMlm)
  PI.vec <- as.vector(PI)
  R2 <- matrix(0, ncol = ncol(PI), nrow = nrow(PI))
  g <- which(gsub("\\.l[[:digit:]]", "", rownames(PI)) %in% cause)
  j <- which(colnames(PI) %in% equation)
  R2[g, j] <- 1
  w <- which(as.vector(R2) != 0)
  N <- length(w)
  R <- matrix(0, ncol = ncol(PI) * nrow(PI), nrow = N)
  for (i in 1:N) R[i, w[i]] <- 1
  sigma.pi <- if (is.null(vcov.)) 
    vcov(xMlm)
  else if (is.function(vcov.)) 
    vcov.(xMlm)
  else vcov.
  df1 <- p * length(y1.names) * length(y2.names)
  df2 <- K * obs - length(PI)
  STATISTIC <- t(R %*% PI.vec) %*% solve(R %*% sigma.pi %*% 
                                           t(R)) %*% R %*% PI.vec/N
  names(STATISTIC) <- "F-Test"
  PARAMETER1 <- df1
  PARAMETER2 <- df2
  names(PARAMETER1) <- "df1"
  names(PARAMETER2) <- "df2"
  PVAL <- 1 - pf(STATISTIC, PARAMETER1, PARAMETER2)
  PARAM <- c(PARAMETER1, PARAMETER2)
  METHOD <- paste("Granger causality H0:", paste(y1.names, 
                                                 collapse = " "), "do not Granger-cause", paste(y2.names, 
                                                                                                collapse = " "))
  result1 <- list(statistic = STATISTIC, parameter = PARAM, 
                  p.value = PVAL, method = METHOD, data.name = paste("VAR object", 
                                                                     obj.name))
  class(result1) <- "htest"
  sigma.u <- crossprod(resid(x))/(obs - ncol(Z))
  colnames(sigma.u) <- y.names
  rownames(sigma.u) <- y.names
  select <- sigma.u[rownames(sigma.u) %in% y2.names, colnames(sigma.u) %in% 
                      y1.names]
  sig.vech <- sigma.u[lower.tri(sigma.u, diag = TRUE)]
  index <- which(sig.vech %in% select)
  N <- length(index)
  Cmat <- matrix(0, nrow = N, ncol = length(sig.vech))
  for (i in 1:N) {
    Cmat[i, index[i]] <- 1
  }
  Dmat <- vars:::.duplicate(K)
  Dinv <- ginv(Dmat)
  lambda.w <- obs %*% t(sig.vech) %*% t(Cmat) %*% solve(2 * 
                                                          Cmat %*% Dinv %*% kronecker(sigma.u, sigma.u) %*% t(Dinv) %*% 
                                                          t(Cmat)) %*% Cmat %*% sig.vech
  STATISTIC <- lambda.w
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- N
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- paste("H0: No instantaneous causality between:", 
                  paste(y1.names, collapse = " "), "and", paste(y2.names, 
                                                                collapse = " "))
  result2 <- list(statistic = STATISTIC, parameter = PARAMETER, 
                  p.value = PVAL, method = METHOD, data.name = paste("VAR object", 
                                                                     obj.name))
  class(result2) <- "htest"
  result2
  return(list(Granger = result1, Instant = result2))
}
