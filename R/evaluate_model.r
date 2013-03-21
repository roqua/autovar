evaluate_model <- function(av_state,model,index) {
  # for debugging
  #av_state$current_model <- model
  
  scat(av_state$log_level,2,"\n",paste(rep('-',times=20),collapse=''),"\n",sep='')
  scat(av_state$log_level,2,index,"/",length(av_state$model_queue),
       ". Model parameters: ",sep='')
  sprint(av_state$log_level,2,model,av_state)
  
  # should return a list with model_valid, and varest
  res <- list(model_valid=TRUE,varest=NULL)
  dta <- get_data_columns(av_state,model)
  av_state <- dta$av_state
  endodta <- dta$endogenous
  exodta <- dta$exogenous
  res$model_valid <- dta$model_valid
  
  # if model does not specify a lag, then this model is invalid,
  # the possibly optimal var orders should be determined (varsoc in STATA),
  # and those models should run again.
  if (is.null(model$lag) || model$lag == -1) {
    res$model_valid <- FALSE
    lags <- determine_var_order(endodta,av_state$log_level,exogen=exodta,lag.max=av_state$lag_max)
    # lags is now a list of possibly optimal VAR orders (integers)
    scat(av_state$log_level,2,"\n> Queueing",length(lags),"VAR model(s) with lags:",lags,"\n")
    for (lag in lags) {
      new_model <- create_model(model,lag=lag)
      av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
    }
    scat(av_state$log_level,2,"\n> End of tests. Did not run tests because no lag specified.\n")
    scat(av_state$log_level,2,paste(rep('-',times=20),collapse=''),"\n\n",sep='')
  } else {
  
    # get the var estimate for this model. This is the 'var' command in STATA.
    res$varest <- run_var(data = endodta, lag = model$lag, exogen=exodta)
    # remove nonsignificant coefficients from the formula
    if (is_restricted_model(model)) {
      #res$varest <- restrict(res$varest,method="ser")
      res$varest <- iterative_restrict(res$varest)
    }
    res$varest <- set_varest_values(av_state,res$varest)
    
    # if we couldn't remove any terms, then the restricted model is equal
    # to the original model, and we set it to invalid because it already exists
    if (is_restricted_model(model) && is.null(res$varest$restrictions)) {
      scat(av_state$log_level,2,"\n> Restricted model equivalent to unrestricted model. Setting as invalid.\n")
      res$model_valid <- FALSE
    }
    
    # help the caching of residuals a bit by using stuff we already computed
    if (is.null(model$exogenous_variables) && !is_restricted_model(model)) {
      av_state <- store_residuals(av_state,model,resid(res$varest))
    }
    
    # run all the tests and queue potential models:
    
    # stability test
    if (!model_is_stable(res$varest,av_state$log_level)) {
      res$model_valid <- FALSE
      # determine whether to continue with this model
    }
    
    # portmanteau tests on residuals and squares of residuals
    ptests <- wntestq(res$varest,av_state$log_level)
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
          if (!apply_log_transform(model) && !is_restricted_model(model)) {
            scat(av_state$log_level,2,"\n> Squares of residuals significant: heteroskedasticity: queueing model with log transform.\n")
            new_model <- create_model(model,apply_log_transform=TRUE,lag=-1)
            av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
          }
        } else if (!test$is_squared && !siflag) {
          siflag <- TRUE
          # autocorrelation in residuals, refine model by adding more lags
          # (done implicitly)
        }
      }
    }
    
    # (Jarque-Bera), Skewness, Kurtosis tests
    vns <- varnorm_aux(res$varest,av_state$log_level)
    if (!is.null(vns)) {
      res$model_valid <- FALSE
      if (!is_restricted_model(model)) {
        scat(av_state$log_level,2,'\n> Skewness/Kurtosis test failed. Queueing model(s) with more strict outlier removal\n')
        if (!apply_log_transform(model)) {
          scat(av_state$log_level,2,"\n> Also queueing model with log transform.\n")
          new_model <- create_model(model,apply_log_transform=TRUE,lag=-1)
          av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
        }
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
            new_exogvars[,] <- new_exogvars[order(new_exogvars$variable),]
          } else {
            new_exogvars <- data.frame(variable=sort(vn),
                                       iteration=rep(1,times=length(vn)),
                                       stringsAsFactors=FALSE)
          }
          if (is.null(old_exogvars) ||
                any(dim(new_exogvars) != dim(old_exogvars)) ||
                any(new_exogvars != old_exogvars)) {
            new_model <- create_model(model,exogenous_variables=new_exogvars,
                                          lag=-1)
            av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
          }
        }
      }
    }
    
    # if all tests pass, print some info about this model
    if (res$model_valid) {
      scat(av_state$log_level,2,'\n> End of tests. Model valid.\n')
      if (!is_restricted_model(model)) {
        scat(av_state$log_level,2,'\n> Queueing model with constraints.\n')
        new_model <- create_model(model,restrict=TRUE)
        av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
      }
      scat(av_state$log_level,1,'Printing estat ic (low values = better models):\n')
      sprint(av_state$log_level,1,estat_ic(res$varest))
    } else {
      scat(av_state$log_level,2,"\n> End of tests. Model invalid.\n")
    }
    scat(av_state$log_level,2,paste(rep('-',times=20),collapse=''),"\n\n",sep='')
  }
  list(res=res,av_state=av_state)
}

# this is a shorter version of evaluate_model, without requeueing.
# This function is used by var_main when test_all_combinations = TRUE
evaluate_model2 <- function(av_state,model,index) {
  # for debugging
  #av_state$current_model <- model
  
  scat(av_state$log_level,2,"\n",paste(rep('-',times=20),collapse=''),"\n",sep='')
  scat(av_state$log_level,2,index,"/",length(av_state$model_queue),
       ". Model parameters: ",sep='')
  sprint(av_state$log_level,2,model,av_state)
  
  # should return a list with model_valid, and varest
  res <- list(model_valid=TRUE,varest=NULL)
  dta <- get_data_columns(av_state,model)
  av_state <- dta$av_state
  endodta <- dta$endogenous
  exodta <- dta$exogenous
  res$model_valid <- dta$model_valid
  
  # get the var estimate for this model. This is the 'var' command in STATA.
  res$varest <- run_var(data = endodta, lag = model$lag, exogen=exodta)
  # remove nonsignificant coefficients from the formula
  if (is_restricted_model(model)) {
    #res$varest <- restrict(res$varest,method="ser")
    res$varest <- iterative_restrict(res$varest)
  }
  res$varest <- set_varest_values(av_state,res$varest)
  
  # if we couldn't remove any terms, then the restricted model is equal
  # to the original model, and we set it to invalid because it already exists
  if (is_restricted_model(model) && is.null(res$varest$restrictions)) {
    scat(av_state$log_level,2,"\n> Restricted model equivalent to unrestricted model. Setting as invalid.\n")
    res$model_valid <- FALSE
  }
  
  # run all the tests and queue potential models:
  
  # stability test
  if (!model_is_stable(res$varest,av_state$log_level)) {
    res$model_valid <- FALSE
    # determine whether to continue with this model
  }
  
  # portmanteau tests on residuals and squares of residuals
  ptests <- wntestq(res$varest,av_state$log_level)
  for (i in 1:(dim(ptests)[1])) {
    test <- ptests[i,]
    if (!test$passes_test) {
      res$model_valid <- FALSE
    }
  }
  
  # (Jarque-Bera), Skewness, Kurtosis tests
  vns <- varnorm_aux(res$varest,av_state$log_level)
  if (!is.null(vns)) {
    res$model_valid <- FALSE
  }
  
  # if all tests pass, print some info about this model
  if (res$model_valid) {
    scat(av_state$log_level,2,'\n> End of tests. Model valid.\n')
    scat(av_state$log_level,1,'Printing estat ic (low values = better models):\n')
    sprint(av_state$log_level,1,estat_ic(res$varest))
  } else {
    scat(av_state$log_level,2,"\n> End of tests. Model invalid.\n")
  }
  scat(av_state$log_level,2,paste(rep('-',times=20),collapse=''),"\n\n",sep='')

  list(res=res,av_state=av_state)
}

determine_var_order <- function(dta,log_level,...) {
  # default type is const
  c <- VARselect(dta,...)
  scat(log_level,1,"\nOptimum lag length according to selection criteria (up to a max of ",
      list(...)$lag.max,"):\n",sep='')
  sprint(log_level,1,c$selection)
  lags <- sort(unique(c$selection))
  lags
}

run_var <- function(data,lag,...) {
  # default type is const
  # (specifies what the rest term in the formula should be)
  m <- VAR(data,p = lag,...)
  m
}

#' Print summary information and tests for a VAR model estimation
#' 
#' This function prints a summary and the output of the tests for a VAR model. The tests it shows are the Eigenvalue stability condition, the Portmanteau tests, the Jarque-Bera tests, the Granger causality Wald tests, and Stata's \code{estat ic}.
#' @param varest an object of class \code{varest}
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity.
#' @examples
#' # av_state is the result of an earlier call to var_main
#' var_info(av_state$accepted_models[[1]]$varest)
#' var_info(av_state$rejected_models[[1]]$varest)
#' @export
var_info <- function(varest,log_level=0) {
  sprint(log_level,1,summary(varest))
  model_is_stable(varest,log_level)
  wntestq(varest,log_level)
  varnorm(varest,log_level)
  varnorm2(varest,log_level)
  vargranger(varest,log_level)
  scat(log_level,2,"\nestat ic\n")
  sprint(log_level,2,estat_ic(varest))
}

model_is_valid <- function(varest,log_level=3) {
  model_is_stable(varest,log_level) &&
    all(wntestq(varest,log_level)$passes_test) &&
    is.null(varnorm_aux(varest,log_level))
}

varnorm_aux <- function(varest,log_level) {
  if (av_state_use_sktest(varest)) {
    varnorm2(varest,log_level)
  } else {
    varnorm(varest,log_level)
  }
}
