# Goodness of fit tests
model_score <- function(varest) {
  # low values == better models
  es <- estat_ic(varest)
  if (av_state_criterion(varest) == 'BIC') {
    es$BIC
  } else {
    es$AIC
  }
}

normalized_model_score <- function(av_state,model) {
  normalized_model <- create_model(model,normalized=TRUE)
  varest <- calc_varest(av_state,normalized_model)
  model_score(varest)
}

printed_model_score <- function(varest) {
  # low values == better models
  es <- estat_ic(varest)
  if (apply_log_transform(varest)) {
    paste("(AIC: ",round(es$AIC,digits=3)," (",round(es$real_AIC,digits=3),")",
          ", BIC: ",round(es$BIC,digits=3)," (",round(es$real_BIC,digits=3),")",")",sep='')
  } else {
    paste("(AIC: ",round(es$AIC,digits=3),
          ", BIC: ",round(es$BIC,digits=3),")",sep='')
  }
}

estat_ic <- function(varest) {
  varsum <- summary(varest)
  nobs <- varsum$obs
  k <- nr_parameters_est(varest)
  if (apply_log_transform(varest)) {
    ll <- logLik_for_logtransformed(varest)
  } else {
    ll <- varsum$logLik
  }
  llreal <- varsum$logLik
  # k = tparms
  # nobs = T
  aic <- -2*ll + 2*k
  realaic <- -2*llreal + 2*k
  bic <- -2*ll + log(nobs)*k
  realbic <- -2*llreal + log(nobs)*k
  res <- data.frame(Obs=nobs,
                    ll=ll,
                    df=k,
                    AIC=aic,
                    BIC=bic,
                    real_AIC=realaic,
                    real_BIC=realbic)
  res
}

logLik_for_logtransformed <- function(object) {
  # http://webspace.qmul.ac.uk/aferreira/lect2-var2_handout.pdf
  # http://www.unc.edu/courses/2010fall/ecol/563/001/docs/lectures/lecture15.htm#transformation
  obs <- object$obs
  K <- object$K
  resids <- resid(object)
  Sigma <- crossprod(resids)/obs
  r <- -(obs * K/2) * log(2 * pi) - (obs/2) * log(det(Sigma)) - 
    (1/2) * sum(diag(resids %*% solve(Sigma) %*% t(resids)))
  r <- r - sum(object$y)
  class(r) <- "logLik"
  return(r)
}

nr_parameters_est <- function(varest) {
  r <- 0
  for (lm in varest$varresult) {
    r <- r+length(lm$coefficients)
  }
  r
}
