# set_var_variables

# NOTE: this function no longer works. it will be removed later.
set_var_variables <- function(vrs,exovrs=NULL,lag.max=10,significance=0.05) {
  # vrs is a list of variables
  cat("\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  cat("Starting VAR with variables:",vrs,"\n")
  exodta <- NULL
  
  models = NULL
  for (lag in lags) {

    
    cat("\nGoodness of fit statistics (low values = better models)\n")
    print(estat_ic(model))

    model_is_stable(model)
    residuals_are_normally_distributed(model)
    wntestq(model)
  }
  model
}




# varnorm <-> Jarque-Bera <-> normality.test()
# The Skewness, Kurtosis, and JB tests (of the residuals)
# cannot be significant

residuals_are_normally_distributed <- function(varest) {
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
  }
  failing_columns <- resid_failing_columns(nt)
  if (!is.null(failing_columns)) {
    cat('  Univariate JB test failed for:',failing_columns,"\n")
  }
  if (is.null(errors) && is.null(failing_columns)) {
    cat("PASS: Unable to reject null hypothesis that residuals are normally distributed.\n")
    TRUE
  } else {
    cat("FAIL: Residuals are significantly not normally distributed.\n")
    FALSE
  }
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


