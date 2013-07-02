# Phillips-Perron Unit Root Test
pperron <- function(av_state,lag,apply_log_transform) {
  scat(av_state$log_level, 2,"\nPhillips-Perron test for unit root\n")
  scat(av_state$log_level, 2,"  apply_log_transform = ",
       apply_log_transform,", lag = ",lag,"\n",sep='')
  if (apply_log_transform) {
    av_state <- add_log_transform_columns(av_state)
  }
  pt <- pperron_test(av_state,lag,apply_log_transform)
  sprint(av_state$log_level,1,pt[c('name','trend_p','trend_p_signif','teststat','five_crit_val','teststat_signif')])
  fail_names <- NULL
  for (i in 1:(dim(pt)[1])) {
    if(i>(dim(pt)[1]) { break }
    test <- pt[i,]
    if (test$needs_trend) {
      fail_names <- c(fail_names,test$name)
    }
  }
  if (is.null(fail_names)) {
    scat(av_state$log_level, 2, "PASS: None of the variables indicate that a trend is significant.\n")
  } else {
    scat(av_state$log_level, 2, "FAIL: Trends are significant for:", 
         paste(fail_names, collapse = ", "), "\n")
  }
  pt
}

pperron_needs_trend_vars <- function(av_state,model) {
  lag <- ifelse(is.null(model$lag),-1,model$lag)
  alt <- apply_log_transform(model)
  pt <- pperron(av_state,lag,alt)
  any(pt$needs_trend)
}

pperron_test <- function(av_state,lag,apply_log_transform) {
  df <- NULL
  for (name in av_state$vars) {
    if (apply_log_transform) { 
      rname <- prefix_ln(name)
    } else {
      rname <- name
    }
    cold <- av_state$data[[av_state$subset]][[name]]
    df <- rbind(df,urppdf(name,rname,av_state,cold,model='trend',lag=lag))
  }
  df
}

urppdf <- function(varname,name,av_state,x,model,lag) {
  if (lag == -1) {
    urpp <- ur.pp(x,type='Z-tau',model=model,lags='short')
  } else {
    urpp <- ur.pp(x,type='Z-tau',model=model,use.lag=lag)
  }
  sum <- urca::summary(urpp)
  if (model == 'trend') {
    trend_p <- sum@testreg$coefficients[rownames(sum@testreg$coefficients)=='trend',][[4]]
    trend_p_signif <- trend_p<=av_state$significance
  } else {
    trend_p <- 0
    trend_p_signif <- TRUE
  }
  five_crit_val <- sum@cval[colnames(sum@cval) == '5pct']
  teststat <- sum@teststat
  needs_trend <- trend_p_signif && (teststat<=five_crit_val)
  df <- data.frame(name=name,
                   varname=varname,
                   model=model,
                   lag=lag,
                   trend_p=trend_p,
                   trend_p_signif=trend_p_signif,
                   teststat=teststat,
                   five_crit_val=five_crit_val,
                   teststat_signif=teststat<=five_crit_val,
                   needs_trend=needs_trend,
                   stringsAsFactors=FALSE)
  df
}
