# set_var_variables

set_var_variables <- function(vrs,exovrs=NULL,lag.max=10,significance=0.05) {
  # vrs is a list of variables
  av_state$significance <<- significance
  dta <- av_state$data[[1]][vrs]
  if (is.null(dta)) {
    stop(paste("invalid endogenous columns specified:",vrs))
  }
  cat("\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  cat("Starting VAR with variables:",vrs,"\n")
  exodta <- NULL
  if (!is.null(exovrs)) {
    exodta <- av_state$data[[1]][exovrs]
    if (is.null(exodta)) {
      stop(paste("invalid exogenous columns specified",exovrs))
    }
    exodta <- as.matrix(exodta)
    colnames(exodta) <- exovrs
    cat("and exogenous variables:",exovrs,"\n")
  }
  
  # Determine how many lags are needed
  # This is the 'varsoc' command in STATA
  # TODO: determine lag.max automatically
  lags <- determine_var_order(dta,exogen=exodta,lag.max=lag.max)
  # lags is now a list of possibly optimal VAR orders (integers)
  cat("Creating",length(lags),"VAR model(s) with lags:",lags,"\n\n")
  
  models = NULL
  for (lag in lags) {
    cat("\n",paste(rep('-',times=20),collapse=''),"\n",sep='')
    cat("Lag length:",lag,"\n")
    # retrieve VAR models. This is the 'var' command in STATA.
    model <- create_var_model(data = dta, lag = lag, exogen=exodta)
    
    cat("\nGoodness of fit statistics\n")
    print(estat_ic(model))

    model_is_stable(model)
    residuals_are_normally_distributed(model)
  }
  model
}

determine_var_order <- function(dta,...) {
  # default type is const
  c <- VARselect(dta,...)
  cat("\nOptimum lag length according to selection criteria (up to a max of ",list(...)$lag.max,"):\n",sep='')
  print(c$selection)
  lags <- sort(unique(c$selection))
  lags
}

create_var_model <- function(data,lag,...) {
  # default type is const
  # (specifies what the rest term in the formula should be)
  m <- VAR(data,p = lag,...)
  m
}


# GOODNESS OF FIT TESTS

is_a_better_model_than <- function(a,b) {
  esa <- estat_ic(a)
  esb <- estat_ic(b)
  esa$AIC+esa$BIC < esb$AIC+esb$BIC
}

estat_ic <- function(varest) {
  varsum <- summary(varest)
  nobs <- varsum$obs
  k <- nr_parameters_est(varest)
  ll <- varsum$logLik
  aic <- -2*ll + 2*k
  bic <- -2*ll + log(nobs)*k
  res <- data.frame(Obs=nobs,
              ll=ll,
              df=k,
              AIC=aic,
              BIC=bic)
  res
}

nr_parameters_est <- function(varest) {
  r <- 0
  for (lm in varest$varresult) {
    r <- r+length(lm$coefficients)
  }
  r
}



# STABILITY TESTS

model_is_stable <- function(varest) {
  vsg <- varstable_graph(varest)
  cat("\nEigenvalue stability condition\n")
  print(vsg)
  if (all(vsg$Modulus < 1)) {
    cat("PASS: All the eigen values lie in the unit circle. VAR satisfies stability condition.\n")
    TRUE
  } else {
    cat("FAIL: Not all the eigen values lie in the unit circle. VAR DOES NOT satisfy stability condition.\n")
    FALSE
  }
}

varstable_graph <- function(varest) {
  nvars <- varest$K
  mat <- matrix(nrow=nvars,ncol=nvars)
  i <- 0
  for (lm in varest$varresult) {
    i <- i+1
    mat[,i] <- lm$coefficients[1:nvars]
  }
  eigvals <- eigen(mat,only.values=TRUE)$values
  ret <- data.frame(Eigenvalue=eigvals,Modulus=Mod(eigvals))
  ret
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
