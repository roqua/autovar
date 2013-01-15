# varnorm <-> Jarque-Bera <-> normality.test()
# The Skewness, Kurtosis, and JB tests (of the residuals)
# cannot be significant

varnorm <- function(varest) {
  ret_fail_columns <- NULL
  errors <- NULL
  nt <- normality.test(varest,multivariate.only=FALSE)
  tests <- c('JB','Skewness','Kurtosis')
  cat("\nJarque-Bera, Skewness, and Kurtosis tests\n")
  for (test in tests) { 
    cat('    p-value for',test,'is',nt$jb.mul[[test]]$p.value[1,1],"\n")
    if (nt$jb.mul[[test]]$p.value[1,1] <= av_state$significance) {
      errors <- c(errors,test)
    }
  }
  if (!is.null(errors)) {
    cat('  Multivariate normality test failed for:',errors,"\n")
    ret_fail_columns <- av_state$vars
  }
  failing_columns <- resid_failing_columns(nt)
  if (!is.null(failing_columns)) {
    cat('  Univariate JB test failed for:',failing_columns,"\n")
    for (column in failing_columns) {
      real_column <- unprefix_ln(column)
      if (!(real_column %in% ret_fail_columns)) {
        ret_fail_columns <- c(ret_fail_columns,real_column)
      }
    }
  }
  if (is.null(errors) && is.null(failing_columns)) {
    cat("PASS: Unable to reject null hypothesis that residuals are normally distributed.\n")
  } else {
    cat("FAIL: Residuals are significantly not normally distributed.\n")
  }
  if (!is.null(ret_fail_columns)) {
    ret_fail_columns <- powerset(ret_fail_columns)
  }
  ret_fail_columns
}

resid_failing_columns <- function(normtest) {
  ret <- NULL
  for (name in names(normtest$jb.uni)) {
    ntr <- normtest$jb.uni[[name]]
    cat('    p-value for JB of',name,'is',ntr$p.value,"\n")
    if (ntr$p.value <= av_state$significance) {
      ret <- c(ret,name)
    }
  }
  ret
}

powerset <- function(lst) {
  res <- NULL
  for (i in 1:length(lst)) {
    cmbs <- combn(lst,i)
    for (j in 1:(dim(cmbs)[[2]])) {
      res <- c(res,list(cmbs[,j]))
    }
  }
  res
}
