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
    
    cat("\nGoodness of fit statistics (low values = better models)\n")
    print(estat_ic(model))

    model_is_stable(model)
    residuals_are_normally_distributed(model)
    wntestq(model)
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



# wntestq <-> Portmeanteau <-> serial.test() <-- not used, rewritten
#   Portmanteau test for white noise
# wntestq in stata, test it on the residuals.
# wntestq mag NIET significant zijn, want dan is de data niet normaal
# -> If the tests for residuals are significant then we have to redefine our model by using more lags
# Significance here means very small (<= 0.05) or very large (>= 0.95)
# -> If the Portmanteau test for the squares of the residuals is significant, there is heteroskedasticity in the series. Then you may consider a logtransformation of your variables.

wntestq <- function(varest) {
  cat("\nPortmanteau tests for white noise\n")
  ptests <- portmanteau_test(varest)
  fail_names <- NULL
  for (i in 1:(dim(ptests)[1])) {
    test <- ptests[i,]
    cat("  ",test$name,':\n',sep="")
    cat('    Portmanteau (Q) statistic = ',test$q,"\n",sep="")
    if (test$passes_test) {  
       cat('    Prob <> chi2(',test$df,') = ',test$p,"\n",sep="")
    } else {
      cat('    Prob <> chi2(',test$df,') = ',test$p," <-- FAILED\n",sep="")
      fail_names <- c(fail_names,test$name)
    }
  }
  if (is.null(fail_names)) {
    cat("PASS: There is no autocorrelation in the residuals.\n")
  } else {
    cat("FAIL: There is still autocorrelation in the residuals for:",fail_names,"\n")
  }
  ptests
}

determine_pt_lags <- function(varest) {
  # this is the default value used in stata
  min(floor(varest$obs/2)-2,40)
}

portmanteau_test <- function(varest) {
  st <- serial.test(varest,lags.pt=determine_pt_lags(varest))
  h <- determine_pt_lags(varest)
  n <- varest$obs
  qs <- NULL
  ps <- NULL
  dfs <- NULL
  ns <- NULL
  ons <- NULL
  isq <- NULL
  pst <- NULL
  for (i in 1:(dim(st$resid)[2])) {
    name <- dimnames(st$resid)[[2]][i]
    data <- st$resid[,i]
    q <- portmanteau_test_statistic(data,n,h)
    p <- chi_squared_prob(q,h)
    qs <- c(qs,q)
    ps <- c(ps,p)
    dfs <- c(dfs,h)
    ns <- c(ns,name)
    ons <- c(ons,name)
    isq <- c(isq,FALSE)
    pst <- c(pst,p > av_state$significance)
    # add the squares of the residuals
    data <- data^2
    q <- portmanteau_test_statistic(data,n,h)
    p <- chi_squared_prob(q,h)
    qs <- c(qs,q)
    ps <- c(ps,p)
    dfs <- c(dfs,h)
    ns <- c(ns,paste(name,'squared'))
    ons <- c(ons,name)
    isq <- c(isq,TRUE)
    pst <- c(pst,p > av_state$significance)
  }
  data.frame(name=ns,q=qs,p=ps,df=dfs,
             variable=ons,is_squared=isq,passes_test=pst,
             stringsAsFactors=FALSE)
}

chi_squared_prob <- function(q,h) {
  # measurement is significant either when very high or very low
  pchisq(q,h,lower.tail=(q <= h))
}
portmanteau_test_statistic <- function(data,n,h) {
  # first we must center the data around zero
  data <- data-mean(data)
  suma <- 0
  for (k in 1:h) {
    suma <- suma + (sample_autocorrelation(data,k,n)^2)/(n-k)
  }
  q <- n*(n+2)*suma
  q
}

sample_autocorrelation <- function(data,k,n) {
  res <- 0
  for (t in (k+1):n) {
    res <- res + data[t]*data[t-k]
  }
  # see paper of Ljung-Box test for this definition of autocorrelation
  denom <- 0
  for (t in 1:n) {
    denom <- denom + data[t]^2
  }
  res <- res/denom
  res
}
