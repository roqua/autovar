# wntestq <-> Portmeanteau <-> serial.test() <-- not used, rewritten
#   Portmanteau test for white noise
# wntestq in stata, test it on the residuals.
# wntestq mag NIET significant zijn, want dan is de data niet normaal
# -> If the tests for residuals are significant then we have to redefine our model by using more lags
# Significance here means very small (<= 0.05) or very large (>= 0.95)
# -> If the Portmanteau test for the squares of the residuals is significant, there is heteroskedasticity in the series. Then you may consider a logtransformation of your variables.

wntestq <- function(varest) {
  scat(2,"\nPortmanteau tests for white noise\n")
  ptests <- portmanteau_test(varest)
  fail_names <- NULL
  for (i in 1:(dim(ptests)[1])) {
    test <- ptests[i,]
    scat(1,"  ",test$name,':\n',sep="")
    scat(1,'    Portmanteau (Q) statistic = ',test$q,"\n",sep="")
    if (test$passes_test) {  
      scat(1,'    Prob > chi2(',test$df,') = ',test$p,"\n",sep="")
    } else {
      scat(1,'    Prob > chi2(',test$df,') = ',test$p," <-- FAILED\n",sep="")
      fail_names <- c(fail_names,test$name)
    }
  }
  if (is.null(fail_names)) {
    scat(2,"PASS: There is no autocorrelation in the residuals.\n")
  } else {
    scat(2,"FAIL: There is still autocorrelation in the residuals for:",
         paste(fail_names,collapse=', '),"\n")
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
  # pchisq(q,h,lower.tail=(q <= h))
  pchisq(q,h,lower.tail=FALSE)
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
