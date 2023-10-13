evaluate_model <- function(av_state,model,index,totmodelcnt) {
  scat(av_state$log_level,2,"\n",paste(rep('-',times=20),collapse=''),"\n",sep='')
  scat(av_state$log_level,2,index,"/",totmodelcnt,
       ". Model parameters: ",sep='')
  sprint(av_state$log_level,2,model,av_state)
  
  # should return a list with model_valid, and varest
  res <- list(model_valid=TRUE,varest=NULL,model=model)
  dta <- get_data_columns(av_state,model)
  av_state <- dta$av_state
  endodta <- dta$endogenous
  exodta <- dta$exogenous
  res$model_valid <- dta$model_valid
  # don't save model if it was invalid because of duplication
  dont_return_model <- !(dta$model_valid)
  
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
    res$varest <- run_var(data = endodta, lag = model$lag, 
                          simple_models = av_state$simple_models, exogen=exodta)
    res$varest <- set_varest_values(av_state,res$varest,model)
    
    # remove nonsignificant coefficients from the formula
    if (is_restricted_model(model)) {
      res$varest <- iterative_restrict(res$varest,
                                       av_state$restrictions.verify_validity_in_every_step,
                                       av_state$restrictions.extensive_search)
      
      # if we couldn't remove any terms, then the restricted model is equal
      # to the original model, and we set it to invalid because it already exists
      if (is.null(res$varest$restrictions)) {
        scat(av_state$log_level,2,"\n> Restricted model equivalent to unrestricted model. Setting as invalid.\n")
        res$model_valid <- FALSE
      }
    }
       
    # run all the tests and queue potential models:
    
    # stability test
    if (!model_is_stable(res$varest,av_state$log_level)) {
      res$model_valid <- FALSE
      # try to toggle the inclusion of trend variables
      av_state <- queue_model_with_or_without_trend(av_state,model)
    }
    
    # portmanteau tests on residuals and squares of residuals
    ptests <- wntestq(res$varest,av_state$log_level)
    fail_names <- NULL
    sqflag <- FALSE
    siflag <- FALSE
    for (i in 1:(dim(ptests)[1])) {
      if (i>(dim(ptests)[1])) { break }
      test <- ptests[i,]
      if (!test$passes_test) {
        res$model_valid <- FALSE
        if (test$is_squared && !sqflag) {
          sqflag <- TRUE
          # squares of residuals significant: heteroskedasticity: apply log transform
          av_state <- queue_model_with_log_transform(av_state,model,
                                                     "\n> Squares of residuals significant: heteroskedasticity: queueing model with log transform.\n")
        } else if (!test$is_squared && !siflag) {
          siflag <- TRUE
          # autocorrelation in residuals, refine model by adding more lags
          # (done implicitly)
        }
      }
    }
    # also queue things with more outliers if we have a test fail
    failing_vars <- ptests[ptests$passes_test == FALSE,]$variable
    if (length(failing_vars) > 0) {
      scat(av_state$log_level,2,'\n> Also queueing model(s) with more strict outlier removal.\n')
      vns <- sapply(unique(failing_vars),unprefix_ln,USE.NAMES=FALSE)
      av_state <- queue_models_with_more_outliers(av_state,model,vns)
    }
    
    # (Jarque-Bera), Skewness, Kurtosis tests
    vns <- varnorm_aux(res$varest,av_state$log_level)
    if (!is.null(vns)) {
      res$model_valid <- FALSE
      if (!is_restricted_model(model)) {
        scat(av_state$log_level,2,'\n> Skewness/Kurtosis test failed. Queueing model(s) with more strict outlier removal.\n')
        # vns is a powerset of vn, minus the empty set
        av_state <- queue_models_with_more_outliers(av_state,model,vns)
        av_state <- queue_model_with_log_transform(av_state,model,
                                                   "\n> Also queueing model with log transform.\n")
      }
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
  }
  if (dont_return_model) {
    res$varest <- NULL
  }
  list(res=res,av_state=av_state)
}

queue_model_with_or_without_trend <- function(av_state,model) {
  if (!is.null(av_state$trend_vars) && av_state$use_pperron && !is_restricted_model(model)) {
    new_model <- NULL
    mmsg <- NULL
    if (!is.null(model$include_trend_vars) && model$include_trend_vars) {
      # toggle to false
      mmsg <- "\n> VAR model is not stable: queueing model without trend variable(s)\n"
      new_model <- create_model(model,include_trend_vars=FALSE,exogenous_variables=NULL)
    } else {
      # toggle to true
      mmsg <- "\n> VAR model is not stable: queueing model with trend variable(s)\n"
      new_model <- create_model(model,include_trend_vars=TRUE,exogenous_variables=NULL)
    }
    scat(av_state$log_level,2,mmsg)
    av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
  }
  av_state
}

queue_model_with_log_transform <- function(av_state,model,message) {
  if (av_state$use_varsoc && !apply_log_transform(model) && !is_restricted_model(model)) {
    scat(av_state$log_level,2,message)
    new_model <- create_model(model,apply_log_transform=TRUE,lag=-1)
    av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
  }
  av_state
}

queue_models_with_more_outliers <- function(av_state,model,vns) {
  if (!is_restricted_model(model)) {
    for (vn in vns) {
      new_exogvars <- NULL
      old_exogvars <- NULL
      if (!is.null(model$exogenous_variables)) {
        old_exogvars <- model$exogenous_variables
        new_exogvars <- old_exogvars
        for (i in 1:nr_rows(old_exogvars)) {
          if (i>nr_rows(old_exogvars)) { break }
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
                                  lag=model_lag(av_state,model))
        av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
      }
    }
  }
  av_state
}

model_lag <- function(av_state,model) {
  if (av_state$use_varsoc) {
    -1
  } else {
    model$lag
  }
}

determine_var_order <- function(dta,log_level,...) {
  # default type is const
  c <- vars::VARselect(dta,...)
  scat(log_level,1,"\nOptimum lag length according to selection criteria (up to a max of ",
      list(...)$lag.max,"):\n",sep='')
  sprint(log_level,1,c$selection)
  lags <- sort(unique(c$selection))
  lags
}

run_var <- function(data,lag,simple_models,...) {
  # default type is const
  # (specifies what the rest term in the formula should be)
  m <- NULL
  if (lag == 0) {
    m <- estimate_var_model(data, 1, ...)
    resmat <- rep.int(1,length(restriction_matrix_colnames(m)))
    resmat[1:(length(colnames(m$y)))] <- 0
    resmat <- rep(resmat,length(colnames(m$y)))
    m <- vars::restrict(m,
             method="manual",
             resmat=format_restriction_matrix(m,resmat))
    m <- add_intercepts(m)
  } else if (lag == 2 && simple_models) {
    m <- estimate_var_model(data, lag, ...)

    # restricting second lag in all models but the one using it
    resmat <- rep.int(1,length(restriction_matrix_colnames(m)))
    nr_vars <- (length(colnames(m$y)))
    resmat[(nr_vars+1):(2*nr_vars)] <- 0
    resmat <- rep(resmat,length(colnames(m$y)))
    resmat[seq(nr_vars+1,length(resmat),length(restriction_matrix_colnames(m))+1)] <- 1
    m <- vars::restrict(m,
                  method="manual",
                  resmat=format_restriction_matrix(m,resmat))
    m <- add_intercepts(m)
  } else {
    m <- estimate_var_model(data, lag, ...)
  }
  full_params <- list(...)
  if (!is.null(full_params$exogen))
    m$exogen <- full_params$exogen
  m
}

estimate_var_model <- function(data, p, ...) {
  # This fix is performed so the call to the VAR function does not include a
  # ..1 whenever all of the ...'s elements are NULL. This matters when
  # performing IRF, as it will use the call function to perform an update of
  # the VAR model.
  if (all(unlist(lapply(list(...), function(x) is.null(x))))) {
    return(vars::VAR(y = data, p = p))
  } 
  # Note the do.call here. Instead of running 
  # vars::VAR(y = data, p = p, ...)
  # we run this do.call in order to expand and explicitly include the ... and
  # other params. This way, when we run the `update` function on the call of 
  # this object, we can rely on all information being available in the object
  # itself (instead of relying on global env).
  return(do.call(vars::VAR, c(list(y=data, p=p), list(...))))
}

add_intercepts <- function(varest) {
  for (i in 1:length(varest$varresult))
    attr(varest$varresult[[i]]$terms,"intercept") <- 1
  varest
}

calc_varest <- function(av_state,model) {
  dta <- get_data_columns(av_state,model)
  av_state <- dta$av_state
  varest <- run_var(data = dta$endogenous, lag = model$lag, 
                    simple_models = av_state$simple_models, exogen=dta$exogenous)
  varest <- set_varest_values(av_state,varest,model)
  if (is_restricted_model(model)) {
    varest <- iterative_restrict(varest,
                                 av_state$restrictions.verify_validity_in_every_step,
                                 av_state$restrictions.extensive_search)
  }
  varest
}

#' Print summary information and tests for a VAR model estimation
#' 
#' This function prints a summary and the output of the tests for a VAR model. The tests it shows are the Eigenvalue stability condition, the Portmanteau tests, the Jarque-Bera tests, the sk tests, the Granger causality Wald tests, and Stata's \code{estat ic}.
#' @param varest an object of class \code{varest}
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity.
#' @examples
#' \dontrun{
#' av_state <- load_file("../data/input/pp1 nieuw compleet.sav",log_level=3)
#' av_state <- var_main(av_state,c('SomBewegUur','SomPHQ'),criterion='BIC',log_level=3)
#' # av_state is the result of a call to var_main
#' var_info(av_state$accepted_models[[1]]$varest)
#' var_info(av_state$rejected_models[[1]]$varest)
#' }
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
