# vargranger
# if the exclusion of variable a is significant for the equation of variable b,
# then a granger causes b.
# also give a one line summary for results

vargranger <- function(varest) {
  # TODO: make this function work when var has more than two variables
  if (dim(varest$y)[[2]] > 2) {
    stop("the current vargranger implementation only works for two variables")
  }
  scat(2,"\nGranger causality Wald tests\n")
  res <- NULL
  if (av_state$small) {
    res <- vargranger_aux_small(varest)
  } else {
    res <- vargranger_aux(varest)
  }
  sprint(1,res)
  tos <- vargranger_to_string(res)
  if (tos != '') {
    scat(2,'Vargranger causes: ',tos,'\n',sep='')
  } else {
    scat(2,'No significant Granger causes detected.\n')
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

vargranger_aux <- function(varest) {
  res <- NULL
  for (eqname in dimnames(varest$y)[[2]]) {
    for (exname in dimnames(varest$y)[[2]]) {
      if (exname == eqname) { next }
      gres <- NULL
      tryCatch(gres <- causality(varest,cause=exname)$Granger,error=function(e) { })
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
      gres <- NULL
      tryCatch(gres <- causality(varest,cause=exname)$Granger,error=function(e) { })
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

vargranger_to_string <- function(res) {
  # res is a vargranger_aux result
  str <- NULL
  for (row in df_in_rows(res)) {
    if (row$P <= av_state$significance) {
      str <- c(str,paste(unprefix_ln(row$Excluded),
                         ' Granger causes ',
                         unprefix_ln(row$Equation),
                         ' (',signif(row$P,digits=3),')',sep=''))
    } else if (row$P <= 2*av_state$significance) {
      str <- c(str,paste(unprefix_ln(row$Excluded),
                         ' almost Granger causes ',
                         unprefix_ln(row$Equation),
                         ' (',signif(row$P,digits=3),')',sep=''))
    }
  }
  if (!is.null(str)) {
    str <- paste(str,collapse='; ')
  } else {
    str <- ''
  }
  str
}

vargranger_line <- function(varest) {
  vargranger_to_string(vargranger_aux(varest))
}
