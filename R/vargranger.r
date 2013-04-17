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
  lst2 <- lst[find_models(lst,list(restrict=FALSE))]
  print_vargranger_list(av_state,lst2,"unrestricted models")
  print_vargranger_list(av_state,av_state$accepted_models,"valid models")
  lst2 <- av_state$accepted_models[find_models(av_state$accepted_models,list(restrict=FALSE))]
  print_vargranger_list(av_state,lst2,"valid unrestricted models")
}

print_vargranger_list <- function(av_state,lst,title) {
  if (length(lst) != 0) {
    scat(av_state$log_level,3,
         paste("\nGranger causality summary of all ",
               length(lst)," ",title,":\n",sep=''))
    glist <- vargranger_list(lst)
    flag <- TRUE
    for (i in 1:nr_rows(glist)) {
      gres <- glist[i,]
      if (gres$desc == '') { gres$desc <- '<None>' }
      scat(av_state$log_level,3,"  ",gres$perc,"\t",gres$desc,"\n",sep='')
      flag <- FALSE
    }
    if (flag) {
      scat(av_state$log_level,3,"  None of the models indicate Granger causality.\n",sep='')
    }
  }
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
      if (is.null(gres)) { next }
      df <- get_named(gres$parameter,'df1')
      chi2 <- gres$statistic[1,1]*df
      P <- chi_squared_prob(chi2,df)
      if (is.null(res)) {
        res <- data.frame(Equation=eqname,
                          Excluded=exname,
                          chi2=chi2,df=df,
                          P=P,
                          stringsAsFactors=FALSE)
      } else {
        res <- rbind(res,list(eqname,exname,chi2,df,P))
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
                          F=F,df=df,df_r=df_r,
                          P=P,
                          stringsAsFactors=FALSE)
      } else {
        res <- rbind(res,list(eqname,exname,F,df,df_r,P))
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

vargranger_line <- function(varest,...) {
  vargranger_to_string(varest,vargranger_call(varest),...)
}

vargranger_list <- function(lst) {
  tbl <- table(sapply(lst,function(x) vargranger_line(x$varest,include_significance=FALSE)))
  tdescs <- names(tbl)
  tfreq <- sum(tbl)
  descs <- NULL
  freqs <- NULL
  percs <- NULL
  for (i in 1:length(tbl)) {
    desc <- tdescs[[i]]
    freq <- tbl[[i]]
    perc <- format_as_percentage(freq/tfreq)
    descs <- c(descs,desc)
    freqs <- c(freqs,freq)
    percs <- c(percs,perc)
  }
  df <- data.frame(desc=descs,freq=freqs,perc=percs,stringsAsFactors=FALSE)
  df <- df[with(df,order(df$freq,decreasing=TRUE)),]
  df
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
