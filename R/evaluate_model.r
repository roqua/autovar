# evaluate_model

evaluate_model <- function(model) {
  # should return a list with model_valid, and varest
  res <- list(model_valid=TRUE,varest=NULL)
  
  
  dta <- get_data_columns(model)
  # this returns a list with fields: endogenous, exogenous
  
  endodta <- dta$endogenous
  exodta <- dta$exogenous
  
  # if model does not specify a lag, then this model is invalid,
  # the possibly optimal var orders should be determined (varsoc in STATA),
  # and those models should run again.
  if (is.null(model$lag)) {
    res$model_valid <- FALSE
    lags <- determine_var_order(endodta,exogen=exodta,lag.max=av_state$lag_max)
    # lags is now a list of possibly optimal VAR orders (integers)
    cat("Creating",length(lags),"VAR model(s) with lags:",lags,"\n\n")
    for (lag in lags) {
      new_model <- create_new_model(model,lag=lag)
      av_state$model_queue <<- add_to_queue(av_state$model_queue,new_model)
    }
  } else {
    cat("\n",paste(rep('-',times=20),collapse=''),"\n",sep='')
    cat("Lag length:",model$lag,"\n")
    
    # get the var estimate for this model. This is the 'var' command in STATA.
    res$varest <- run_var(data = endodta, lag = model$lag, exogen=exodta)
    
    
    # run all the tests and queue potential models...
    
    
  }
  res
}

determine_var_order <- function(dta,...) {
  # default type is const
  c <- VARselect(dta,...)
  cat("\nOptimum lag length according to selection criteria (up to a max of ",
      list(...)$lag.max,"):\n",sep='')
  print(c$selection)
  lags <- sort(unique(c$selection))
  lags
}

run_var <- function(data,lag,...) {
  # default type is const
  # (specifies what the rest term in the formula should be)
  m <- VAR(data,p = lag,...)
  m
}
