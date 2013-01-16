# evaluate_model

evaluate_model <- function(model) {
  # for debugging
  av_state$current_model <<- model
  
  # should return a list with model_valid, and varest
  res <- list(model_valid=TRUE,varest=NULL)
  dta <- get_data_columns(model)
  # this returns a list with fields: endogenous, exogenous
  
  endodta <- dta$endogenous
  exodta <- dta$exogenous
  
  # if model does not specify a lag, then this model is invalid,
  # the possibly optimal var orders should be determined (varsoc in STATA),
  # and those models should run again.
  if (is.null(model$lag) || model$lag == -1) {
    res$model_valid <- FALSE
    lags <- determine_var_order(endodta,exogen=exodta,lag.max=av_state$lag_max)
    # lags is now a list of possibly optimal VAR orders (integers)
    cat("> Queueing",length(lags),"VAR model(s) with lags:",lags,"\n\n")
    for (lag in lags) {
      new_model <- create_new_model(model,lag=lag)
      av_state$model_queue <<- add_to_queue(av_state$model_queue,new_model)
    }
  } else {
    cat("\n",paste(rep('-',times=20),collapse=''),"\n",sep='')
    cat("Model parameters: ")
    print(model)
    
    # get the var estimate for this model. This is the 'var' command in STATA.
    res$varest <- run_var(data = endodta, lag = model$lag, exogen=exodta)
    
    # run all the tests and queue potential models...
    
    # stability test
    if (!model_is_stable(res$varest)) {
      res$model_valid <- FALSE
      # determine whether to continue with this model
    }
    
    # portmanteau tests on residuals and squares of residuals
    ptests <- wntestq(res$varest)
    fail_names <- NULL
    sqflag <- FALSE
    siflag <- FALSE
    for (i in 1:(dim(ptests)[1])) {
      test <- ptests[i,]
      if (!test$passes_test) {
        res$model_valid <- FALSE
        if (test$is_squared && !sqflag) {
          sqflag <- TRUE
          # squares of residuals significant: heteroskedasticity: apply log transform
          if (!apply_log_transform(model)) {
            cat("\n> Squares of residuals significant: heteroskedasticity: queueing model with log transform.\n")
            new_model <- create_new_model(model,apply_log_transform=TRUE,lag=-1)
            av_state$model_queue <<- add_to_queue(av_state$model_queue,new_model)
          }
        } else if (!test$is_squared && !siflag) {
          siflag <- TRUE
          # autocorrelation in residuals, refine model by adding more lags
          # (done implicitly)
        }
      }
    }
    
    # Jarque-Bera, Skewness, Kurtosis tests
    vns <- varnorm(res$varest)
    if (!is.null(vns)) {
      res$model_valid <- FALSE
      cat('\n> JB test failed. Queueing model(s) with more strict outlier removal\n')
      # vns is a powerset of vn, minus the empty set
      for (vn in vns) {
        new_exogvars <- NULL
        old_exogvars <- NULL
        if (!is.null(model$exogenous_variables)) {
          old_exogvars <- model$exogenous_variables
          new_exogvars <- old_exogvars
          for (i in 1:nr_rows(old_exogvars)) {
            exovar <- old_exogvars[i,]
            if (exovar$variable %in% vn) {
              new_exogvars[i,]$iteration <- min(exovar$iteration +1,
                                                av_state$exogenous_max_iterations)
            }
          }
          for (name in vn) {
            if (!(name %in% old_exogvars$variable)) {
              new_exogvars <- rbind(new_exogvars,rep(NA,times=dim(new_exogvars)[[2]]))
              new_exogvars[dim(new_exogvars)[[1]],][['variable']] <- name
              new_exogvars[dim(new_exogvars)[[1]],][['iteration']] <- 1
            }
          }
        } else {
          new_exogvars <- data.frame(variable=vn,
                                     iteration=rep(1,times=length(vn)),
                                     stringsAsFactors=FALSE)
        }
        if (is.null(old_exogvars) || 
              dim(new_exogvars) != dim(old_exogvars) || 
              new_exogvars != old_exogvars) {
          new_model <- create_new_model(model,exogenous_variables=new_exogvars,
                                        lag=-1)
          av_state$model_queue <<- add_to_queue(av_state$model_queue,new_model)
        }
      }
    }
    
    # if all tests pass, print some info about this model
    if (res$model_valid) {
      cat('\n> End of tests. Model valid.\n Printing estat ic (low values = better models):\n')
      print(estat_ic(res$varest))
    } else {
      cat("\n> End of tests. Model invalid.\n")
    }
    cat(paste(rep('-',times=20),collapse=''),"\n\n",sep='')
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

print_tests <- function(varest) {
 model_is_stable(varest)
 wntestq(varest)
 varnorm(varest)
}
