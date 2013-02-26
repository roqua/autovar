# Granger causality
vargranger <- function(varest,log_level=0) {
  # TODO: make this function work when var has more than two variables
  if (dim(varest$y)[[2]] > 2) {
    stop("the current vargranger implementation only works for two variables")
  }
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
      gres <- granger_causality(varest,exname)
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
      gres <- granger_causality(varest,exname)
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

granger_causality <- function(varest,cause) {
  varest <- process_restricted_varest(varest,cause)
  gres <- NULL
  tryCatch(gres <- causality(varest,cause=cause)$Granger,error=function(e) { })
  gres
}

process_restricted_varest <- function(varest, cause) {
  if (!is.null(varest$restrictions)) {
    restricts <- varest$restrictions[cause,]
    excluded_names <- names(restricts)[which(restricts == 0)]
    # 0 means exclude
    varest$datamat <- varest$datamat[, !(colnames(varest$datamat) %in% excluded_names)]
  }
  varest
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
  data.frame(desc=descs,freq=freqs,perc=percs,stringsAsFactors=FALSE)
}

format_as_percentage <- function(frac) {
  paste(round(100*frac,digits=2),"%",sep='')
}
