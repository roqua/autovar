#' Determine possibly optimal models for Vector Autoregression
#'
#' This function generates and tests possible VAR models for the specified variables. The only required arguments are \code{av_state} and \code{vars}.
#' @param av_state an object of class \code{av_state}
#' @param vars the vector of variables on which to perform vector autoregression. These should be the names of existing columns in the data sets of \code{av_state}.
#' @param lag_max limits the highest possible number of lags that will be used in a model. This number sets the maximum limit in the search for optimal lags.
#' @param significance the maximum P-value for which results are seen as significant. This argument is used in Granger causality tests, Portmanteau tests, and Jarque-Bera tests.
#' @param exogenous_max_iterations determines how many times we should try to exclude additional outliers for a variable. This argument should be a number between 1 and 3: \itemize{
#' \item \code{1} - When Jarque-Bera tests fail, having \code{exogenous_max_iterations = 1} will only try with removing 3.5x std. outliers for the residuals of variables using exogenous dummy variables.
#' \item \code{2} - When \code{exogenous_max_iterations = 2}, the program will also try with removing 3x std. outliers if JB tests still fail.
#' \item \code{3} - When \code{exogenous_max_iterations = 3}, the program will also try with removing 2.5x std. outliers if JB tests still fail.
#' }
#' @param subset specifies which data subset the VAR analysis should run on. The VAR analysis only runs on one data subset at a time. If not specified, the first subset is used (corresponding to \code{av_state$data[[1]]}).
#' @param log_level sets the minimum level of output that should be shown. It should be a number between 0 and 3. A lower level means more verbosity. \code{0} = debug, \code{1} = test detail, \code{2} = test outcomes, \code{3} = normal. The default is set to the value of \code{av_state$log_level} or if that doesn't exist, to \code{0}. If this argument was specified, the original value of \code{av_state$log_level} is be restored at the end of \code{var_main}.
#' @param small corresponds to the \code{small} argument of Stata's \code{var} function, and defaults to \code{FALSE}. This argument affects the outcome of the Granger causality test. When \code{small = TRUE}, the Granger causality test uses the F-distribution to gauge the statistic. When \code{small = FALSE}, the Granger causality test uses the Chi-squared distribution to gauge the statistic.
#' @param include_model can be used to forcibly include a model in the evaluation. Included models have to be lists, and can specify the parameters \code{lag}, \code{exogenous_variables}, and \code{apply_log_transform}. For example: \code{
#' av_state <- var_main(av_state,c('Activity_hours','Depression'),
#'                      log_level=3,
#'                      small=TRUE,
#'                      include_model=list(lag=3,
#'                         exogenous_variables=data.frame(variable="Depression",
#'                         iteration=1,stringsAsFactors=FALSE),
#'                         apply_log_transform=TRUE))
#' var_info(av_state$rejected_models[[1]]$varest)
#' }
#' The above example includes a model with \code{lag=3} (so lags 1, 2, and 3 are included), the model is ran on the log-transformed variables, and includes an exogenous dummy variable that has a 1 where values of \code{log(Depression)} are more than 3.5xstd away from the mean (because \code{iteration=1}, see the description of the \code{exogenous_max_iterations} parameter above for the meaning of the iterations) and 0 everywhere else. The included model is added at the start of the list, so it can be retrieved (assuming a valid \code{lag} was specified) with either \code{av_state$accepted_models[[1]]} if the model was valid or \code{av_state$rejected_models[[1]]} if it was invalid. In the above example, some info about the included model is printed (assuming it was invalid).
#' @param exogenous_variables should be a vector of variable names that already exist in the given data set, that will be supplied to every VAR model as exogenous variables.
#' @param use_sktest affects which test is used for Skewness and Kurtosis testing of the residuals. When \code{use_sktest = TRUE} (the default), STATA's \code{sktest} is used. When \code{use_sktest = FALSE}, STATA's \code{varnorm} (i.e., the Jarque-Bera test) is used.
#' @param test_all_combinations determines whether the remaining search space is searched for possible additional models. This can sometimes give a few extra solutions at a large performance penalty.
#' @param restrictions.verify_validity_in_every_step is an argument that affects how constraints are found for valid models. When this argument is \code{TRUE} (the default), all intermediate models in the iterative constraint-finding method have to be valid. This ensures that we always find a valid constrained model for every valid model. If this argument is \code{FALSE}, then only after setting all constraints do we check if the resulting model is valid. If this is not the case, we fail to find a constrained model.
#' @param restrictions.extensive_search is an argument that affects how constraints are found for valid models. When this argument is \code{TRUE} (the default), when the term with the highest p-value does not provide a model with a lower BIC score, we attempt to constrain the term with the second highest p-value, and so on. When this argument is \code{FALSE}, we only check the term with the highest p-value. If restricting this term does not give an improvement in BIC score, we stop restricting the model entirely.
#' @param criterion is the information criterion used to sort the models. Valid options are  \code{'AIC'} (the default) or \code{'BIC'}.
#' @param use_varsoc determines whether VAR lag order selection criteria should be employed to restrict the search space for VAR models. When \code{use_varsoc} is \code{FALSE}, all lags from 1 to \code{lag_max} are searched.
#' @param use_pperron determines whether the Phillips-Perron test should be used to determine whether trend variables should be included in the models. When \code{use_pperron} is \code{FALSE}, all models will be evaluated both with and without the trend variable. The trend variable is specified using the \code{\link{order_by}} function.
#' @param include_squared_trend determines whether the square of the trend is included if the trend is included for a model. The trend variable is specified using the \code{\link{order_by}} function.
#' @return This function returns the modified \code{av_state} object. The lists of accepted and rejected models can be retrieved through \code{av_state$accepted_models} and \code{av_state$rejected_models}. To print these, use \code{print_accepted_models(av_state)} and \code{print_rejected_models(av_state)}.
#' @examples
#' av_state <- load_file("../data/input/Activity and depression pp5 Angela.dta")
#' av_state <- group_by(av_state,'id')
#' av_state <- order_by(av_state,'Day')
#' av_state <- add_derived_column(av_state,'Activity_hours','Activity',
#'                                operation='MINUTES_TO_HOURS')
#' av_state <- var_main(av_state,c('Activity_hours','Depression'),log_level=3)
#' var_info(av_state$accepted_models[[1]]$varest)
#' @export
var_main <- function(av_state,vars,lag_max=2,significance=0.05,
                     exogenous_max_iterations=2,subset=1,log_level=av_state$log_level,
                     small=FALSE,include_model=NULL,exogenous_variables=NULL,
                     use_sktest=TRUE,test_all_combinations=FALSE,
                     restrictions.verify_validity_in_every_step=TRUE,
                     restrictions.extensive_search=TRUE,
                     criterion=c('AIC','BIC'),
                     use_varsoc=FALSE,use_pperron=TRUE,
                     include_squared_trend=FALSE) {
  assert_av_state(av_state)
  # lag_max is the global maximum lags used
  # significance is the limit
  # exogenous_max_iterations is the maximum number individual outliers that can be removed
  # subset is the chosen subset of data
  # log_level is the minimum log level that will be printed
  # (0 = debug, 1 = test detail, 2= test outcomes, 3 = normal)
  if (is.null(log_level)) { log_level <- 0 }
  if (!(log_level %in% 0:4)) {
    stop(paste("log_level needs to be in 0:4"))
  }
  if (!(exogenous_max_iterations %in% 1:3)) {
    stop(paste("exogenous_max_iterations needs to be in 1:3"))
  }
  real_log_level <- av_state$log_level
  
  av_state$significance <- significance  
  av_state$lag_max <- lag_max
  av_state$exogenous_max_iterations <- exogenous_max_iterations
  av_state$vars <- vars
  av_state$subset <- subset
  av_state$log_level <- log_level
  av_state$small <- small
  av_state$use_sktest <- use_sktest
  av_state <- add_exogenous_variables(av_state,exogenous_variables)
  av_state$test_all_combinations <- test_all_combinations
  av_state$restrictions.verify_validity_in_every_step <- restrictions.verify_validity_in_every_step
  av_state$restrictions.extensive_search <- restrictions.extensive_search
  av_state$criterion <- match.arg(criterion)
  av_state$use_varsoc <- use_varsoc
  av_state$use_pperron <- use_pperron
  av_state$include_squared_trend <- include_squared_trend

  scat(av_state$log_level,3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  
  # print non-default arguments
  rcall <- 'av_state'
  forms <- formals()
  for (name in names(forms)) {
    if (name == 'av_state') { next }
    curarg <- eval(parse(text=name))
    if (deparse(forms[[name]]) != deparse(curarg)) {
      rcall <- c(rcall,paste(name,' = ',deparse(curarg),sep=''))
    }
  }
  rcall <- paste(rcall,collapse=', ')
  rcall <- paste('var_main(',rcall,')\n\n',sep='')
  scat(av_state$log_level,3,rcall)

  scat(av_state$log_level,3,"Starting VAR with variables: ",paste(vars,collapse=', '),
       ", for subset: ",subset,"\n",sep='')
  
  if (!is.null(av_state$trend_vars) && av_state$include_squared_trend) {
    trend_var <- av_state$trend_vars[[1]]
    sq_name <- paste(trend_var,'2',sep='')
    av_state$trend_vars <- c(trend_var,sq_name)
  }
  
  # check if subset exists
  if (is.null(av_state$data[[av_state$subset]])) {
    stop(paste("invalid subset specified:",av_state$subset))
  }
  
  # check if endogenous columns exist
  for (varname in av_state$vars) {
    if (!(varname %in% names(av_state$data[[av_state$subset]]))) {
      stop(paste("non-existant endogenous column specified:",varname))
    }
  }
  
  # check if exogenous columns exist
  for (varname in unique(c(av_state$exogenous_variables,av_state$day_dummies,av_state$trend_vars))) {
    if (!(varname %in% names(av_state$data[[av_state$subset]]))) {
      stop(paste("non-existant exogenous column specified:",varname))
    }
  }
  
  # make sure that the VAR columns are of the numeric type
  for (mvar in av_state$vars) {
    if (class(av_state$data[[av_state$subset]][[mvar]]) != "numeric") {
      scat(av_state$log_level,2,"column",mvar,"is not numeric, converting...\n")
      av_state$data[[av_state$subset]][[mvar]] <- 
        as.numeric(av_state$data[[av_state$subset]][[mvar]])
    }
  }
  
  # make sure that the exogenous columns are of the numeric type
  for (mvar in unique(c(av_state$exogenous_variables,av_state$day_dummies,av_state$trend_vars))) {
    if (class(av_state$data[[av_state$subset]][[mvar]]) != "numeric") {
      scat(av_state$log_level,2,"column",mvar,"is not numeric, converting...\n")
      tv <- as.numeric(av_state$data[[av_state$subset]][[mvar]])
      av_state$data[[av_state$subset]][[mvar]] <- tv-min(tv)
    }
  }
  
  default_model <- list()
  class(default_model) <- 'var_model'
  
  if (av_state$use_varsoc) {
    av_state$model_queue <- list(default_model)
  } else {
    av_state$model_queue <- NULL
    av_state <- add_log_transform_columns(av_state)
    for (lag in 1:(av_state$lag_max)) {
      new_model <- create_model(default_model,lag=lag)
      av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
      new_model <- create_model(default_model,apply_log_transform=TRUE,lag=lag)
      av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
    }
  }
  if (!is.null(av_state$trend_vars)) {
    model_queue <- av_state$model_queue
    if (av_state$use_pperron) {
      av_state$model_queue <- NULL
      for (model in model_queue) {
        if (pperron_needs_trend_vars(av_state,model)) {
          model <- create_model(model,include_trend_vars=TRUE)
        }
        av_state$model_queue <- add_to_queue(av_state$model_queue,model,av_state$log_level)
      }
    } else {
      # add both
      for (model in model_queue) {
        new_model <- create_model(model,include_trend_vars=TRUE)
        av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
      }
    }
  }
  if (!is.null(av_state$day_dummies)) {
    model_queue <- av_state$model_queue
    for (model in model_queue) {
      new_model <- create_model(model,include_day_dummies=TRUE)
      av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
    }
  }
  if (!is.null(include_model)) {
    if (class(include_model) != 'list') {
      stop(paste("the include_model object has to be of class list"))
    }
    new_model <- merge_lists(default_model,include_model)
    class(new_model) <- "var_model"
    av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
  }
  av_state$accepted_models <- list()
  av_state$rejected_models <- list()
  accepted_models <- list()
  rejected_models <- list()
  av_state$resids <- list()
  av_state$log_resids <- list()
  av_state$model_cnt <- 0
  i <- 1
  pm <- NULL
  while (TRUE) {
    if (i > length(av_state$model_queue)) { break }
    pm <- process_model(av_state,av_state$model_queue[[i]],i)
    av_state <- pm$av_state
    if (!is.null(pm$accepted_model)) {
      accepted_models <- c(accepted_models,list(pm$accepted_model))
    } else if (!is.null(pm$rejected_model)) {
      rejected_models <- c(rejected_models,list(pm$rejected_model))
    }
    i <- i+1
  }
  # first, clean up the valid models
  accepted_models <- remove_duplicates(av_state,accepted_models)
  # now queue the valid models again with constraints
  scat(av_state$log_level,2,'\n> Done. Queueing the valid models again with constraints...\n')
  for (model in accepted_models) {
    new_model <- create_model(model$parameters,restrict=TRUE)
    av_state$model_queue <- add_to_queue(av_state$model_queue,new_model,av_state$log_level)
  }
  # process the valid models with constraints
  while (TRUE) {
    if (i > length(av_state$model_queue)) { break }
    pm <- process_model(av_state,av_state$model_queue[[i]],i)
    av_state <- pm$av_state
    if (!is.null(pm$accepted_model)) {
      accepted_models <- c(accepted_models,list(pm$accepted_model))
    } else if (!is.null(pm$rejected_model)) {
      rejected_models <- c(rejected_models,list(pm$rejected_model))
    }
    i <- i+1
  }
  av_state$accepted_models <- accepted_models
  av_state$rejected_models <- rejected_models
  
  scat(av_state$log_level,3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  class(av_state$accepted_models) <- 'model_list'
  class(av_state$rejected_models) <- 'model_list'
  if (length(av_state$accepted_models) > 0) {
    av_state$accepted_models <- sort_models(av_state$accepted_models)
  }
  var_summary(av_state,
              paste("\nDone. Processed",av_state$model_cnt,"models, of which",
                    length(av_state$accepted_models),
                    ifelse(length(av_state$accepted_models) == 1,"was","were"),"valid.\n"))
  if (av_state$test_all_combinations) {
    scat(av_state$log_level,3,"\nRunning again on full scope")
    accepted_models <- av_state$accepted_models
    rejected_models <- av_state$rejected_models
    av_state$accepted_models <- list()
    av_state$rejected_models <- list()
    old_model_queue <- av_state$model_queue
    model_queue <- create_total_model_queue(av_state)
    model_queue <- model_queue[!(model_queue %in% old_model_queue)]
    av_state$model_queue <- model_queue
    av_state$old_accepted_models <- av_state$accepted_models
    av_state$model_cnt <- 0
    i <- 1
    while (TRUE) {
      if (i > length(av_state$model_queue)) { break }
      pm <- process_model(av_state,av_state$model_queue[[i]],i)
      av_state <- pm$av_state
      if (!is.null(pm$accepted_model)) {
        accepted_models <- c(accepted_models,list(pm$accepted_model))
      } else if (!is.null(pm$rejected_model)) {
        rejected_models <- c(rejected_models,list(pm$rejected_model))
      }
      i <- i+1
    }
    av_state$accepted_models <- accepted_models
    av_state$rejected_models <- rejected_models
    
    scat(av_state$log_level,3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
    class(av_state$accepted_models) <- 'model_list'
    class(av_state$rejected_models) <- 'model_list'
    if (length(av_state$accepted_models) > 0) {
      av_state$accepted_models <- sort_models(av_state$accepted_models)
    }
    var_summary(av_state,
                paste("\nDone. Processed",av_state$model_cnt,"additional models, of which",
                      length(av_state$accepted_models) - length(av_state$old_accepted_models),
                      ifelse(length(av_state$accepted_models) - length(av_state$old_accepted_models) == 1,
                             "was","were"),"valid.\n"))
  }
  av_state$log_level <- real_log_level
  av_state
}

process_model <- function(av_state,model,i) {
  ev_modelres <- evaluate_model(av_state,model,i)
  av_state <- ev_modelres$av_state
  model_evaluation <- ev_modelres$res
  model <- model_evaluation$model
  if (!is.null(model_evaluation$varest)) {
    av_state$model_cnt <- av_state$model_cnt +1
  }
  accepted_model <- NULL
  rejected_model <- NULL
  if (model_evaluation$model_valid) {
    # model is valid
    accepted_model <- list(parameters=model,varest=model_evaluation$varest)
    class(accepted_model) <- 'var_modelres'
  } else if (!is.null(model_evaluation$varest)) {
    # model was rejected
    rejected_model <- list(parameters=model,varest=model_evaluation$varest)
    class(rejected_model) <- 'var_modelres'
  }
  list(av_state=av_state,accepted_model=accepted_model,rejected_model=rejected_model)
}

remove_duplicates <- function(av_state,lst) {
  lst <- add_expanded_models(lst,av_state)
  rlst <- lst
  for (model in lst) {
    if (list(model) %in% rlst) {
      rlst <- remove_included_models(rlst,model$expanded_model)
      rlst <- c(rlst,list(model))
    }
  }
  rlst <- remove_expanded_models(rlst)
  rlst
}

add_expanded_models <- function(lst,av_state) {
  r <- list()
  for (model in lst) {
    model$expanded_model <- expand_model(model$parameters,av_state)
    r <- c(r,list(model))
  }
  r
}

remove_expanded_models <- function(lst) {
  r <- list()
  for (model in lst) {
    model$expanded_model <- NULL
    r <- c(r,list(model))
  }
  r
}

remove_included_models <- function(lst,exp_model) {
  rlst <- list()
  for (tmodel in lst) {
    texp_model <- tmodel$expanded_model
    if (!model_is_the_same_or_needs_fewer_outliers_than(exp_model,texp_model)) {
      rlst <- c(rlst,list(tmodel))
    }
  }
  rlst
}

model_is_the_same_or_needs_fewer_outliers_than <- function(exp_model_a,exp_model_b) {
  if (!models_have_equal_params(exp_model_a[[1]],exp_model_b[[1]])) {
    FALSE
  } else {
    flag <- TRUE
    for (amodel in exp_model_a) {
      found_in_b <- FALSE
      for (bmodel in exp_model_b) {
        if (all(amodel$exogenous_variables %in% bmodel$exogenous_variables)) {
          found_in_b <- TRUE
          break
        }
      }
      if (!found_in_b) {
        flag <- FALSE
        break
      }
    }
    flag
  }
}
  
models_have_equal_params <- function(amodel,bmodel) {
  # amodel and bmodel are instances of expanded models
  amodel$exogenous_variables <- NULL
  bmodel$exogenous_variables <- NULL
  lists_are_equal(amodel,bmodel)
}

expand_model <- function(model_params,av_state) {
  model_params <- merge_lists(default_model_props(),model_params)
  r <- list()
  if (is.null(model_params$exogenous_variables)) {
    r <- c(r,list(model_params))
  } else {
    for (i in 1:(dim(model_params$exogenous_variables)[1])) {
      exorow <- model_params$exogenous_variables[i,]
      exovec <- get_outliers_as_vector(av_state,exorow$variable,exorow$iteration,model_params)
      new_model <- merge_lists(model_params,list(exogenous_variables=exovec))
      r <- c(r,list(new_model))
    }
  }
  r 
}

default_model_props <- function() {
  list(lag = -1,
       apply_log_transform = FALSE,
       include_day_dummies = FALSE,
       include_trend_vars = FALSE,
       restrict = FALSE,
       exogenous_variables = NULL)
}

#' Print the output of var_main
#' 
#' This function repeats the output that is shown after a call of var_main.
#' @param av_state an object of class \code{av_state} that was the result of a call to \code{\link{var_main}}
#' @param msg an optional message to display at the start. If this argument is \code{NULL}, a default message is shown instead.
#' @examples
#' # av_state is the result of a call to var_main
#' var_summary(av_state)
#' @export
var_summary <- function(av_state,msg=NULL) {
  if (is.null(msg)) {
    model_cnt <- length(av_state$accepted_models)+length(av_state$rejected_models)
    msg <- paste("\nProcessed",model_cnt,"models, of which",
                 length(av_state$accepted_models),
                 ifelse(length(av_state$accepted_models) == 1,"was","were"),"valid.\n")
  }
  scat(av_state$log_level,3,msg)
  search_space_used(av_state)
  print_granger_statistics(av_state)
  vargranger_plot(av_state)
  print_model_statistics(av_state)
  if (length(av_state$accepted_models) > 0) {
    scat(av_state$log_level,3,"\nThe valid models (sorted by",av_state$criterion,"score):\n")
    scat(av_state$log_level,3,format_accepted_models(av_state))
  }
}

print_best_models <- function(av_state) {
  logm <- find_models(av_state$accepted_models,list(apply_log_transform = TRUE))
  if (!is.null(logm)) {
    best_log <- av_state$accepted_models[[logm[[1]]]]
    scat(av_state$log_level,3,"\n",paste(rep('-',times=53),collapse=''),"\n",sep='')
    scat(av_state$log_level,3,"Details for the best log-transformed model (model ",
         idx_chars(logm[[1]]),"):\n",sep='')
    scat(av_state$log_level,3,paste(rep('-',times=53),collapse=''),"\n",sep='')
    var_info(best_log$varest)
  }
  non_logm <- find_models(av_state$accepted_models,list(apply_log_transform = FALSE))
  if (!is.null(non_logm)) {
    best_non_log <- av_state$accepted_models[[non_logm[[1]]]]
    scat(av_state$log_level,3,"\n\n",paste(rep('-',times=59),collapse=''),"\n",sep='')
    scat(av_state$log_level,3,"Details for the best model without log-transform (model ",
         idx_chars(non_logm[[1]]),"):\n",sep='')
    scat(av_state$log_level,3,paste(rep('-',times=59),collapse=''),"\n",sep='')
    var_info(best_non_log$varest)
  }
}

set_varest_values <- function(av_state,varest) {
  varest$small <- av_state$small
  varest$significance <- av_state$significance
  varest$vars <- av_state$vars
  varest$use_sktest <- av_state$use_sktest
  varest$criterion <- av_state$criterion
  varest
}

av_state_small <- function(varest) {
  if (!is.null(varest$small)) {
    varest$small
  } else {
    FALSE
  }
}

av_state_significance <- function(varest) {
  if (!is.null(varest$significance)) {
    varest$significance
  } else {
    0.05
  }
}

av_state_vars <- function(varest) {
  if (!is.null(varest$vars)) {
    varest$vars
  } else {
    names(varest$varresult)
  }
}

av_state_use_sktest <- function(varest) {
  if (!is.null(varest$use_sktest)) {
    varest$use_sktest
  } else {
    TRUE
  }
}

av_state_criterion <- function(varest) {
  if (!is.null(varest$criterion)) {
    varest$criterion
  } else {
    'AIC'
  }
}

add_log_transform_columns <- function(av_state) {
  for (name in av_state$vars) {
    ln_name <- prefix_ln(name)
    if (!column_exists(av_state,ln_name)) {
      av_state <- add_derived_column(av_state,
                                     ln_name,
                                     name,
                                     operation='LN',
                                     log_level=av_state$log_level)
    }
  }
  av_state
}

add_exogenous_variables <- function(av_state,column_names) {
  av_state$exogenous_variables <- unique(c(av_state$exogenous_variables,column_names))
  av_state
}

print_model_statistics <- function(av_state) {
  if (length(av_state$accepted_models) > 0) {
    scat(av_state$log_level, 3, "\nSummary of all valid models:\n")
    print_model_statistics_aux(av_state,av_state$accepted_models,'lag')
    scat(av_state$log_level,3,"\n")
    print_model_statistics_aux(av_state,av_state$accepted_models,'apply_log_transform')
    if (!is.null(av_state$day_dummies)) {
      scat(av_state$log_level,3,"\n")
      print_model_statistics_aux(av_state,av_state$accepted_models,'include_day_dummies')
    }
    if (!is.null(av_state$trend_vars)) {
      scat(av_state$log_level,3,"\n")
      print_model_statistics_aux(av_state,av_state$accepted_models,'include_trend_vars')
    }
  } else if (length(av_state$rejected_models) > 0 && (!is.null(av_state$day_dummies) || !is.null(av_state$trend_vars))) {
    scat(av_state$log_level, 3, "\nSummary of all rejected models:")
    if (!is.null(av_state$day_dummies)) {
      scat(av_state$log_level,3,"\n")
      print_model_statistics_aux(av_state,av_state$rejected_models,'include_day_dummies')
    }
    if (!is.null(av_state$trend_vars)) {
      scat(av_state$log_level,3,"\n")
      print_model_statistics_aux(av_state,av_state$rejected_models,'include_trend_vars')
      if (is.null(av_state$day_dummies) && length(find_models(av_state$rejected_models,list(include_trend_vars=TRUE))) > 0) {
        scat(av_state$log_level,3,"\nTo find valid models, try using the set_timestamps() function so the VAR models can include days and day parts as dummy variables.\n")
      }
    }
    if (av_state$exogenous_max_iterations < 3) {
      scat(av_state$log_level,3,"\nTo find valid models, try running var_main again with 'exogenous_max_iterations=3'.\n")
    }
    scat(av_state$log_level,3,"\n")
  }
}

print_model_statistics_aux <- function(av_state,lst,param) {
  scat(av_state$log_level, 3, paste("  ",param,":\n",sep=''))
  glist <- model_statistics(lst,param)
  for (i in 1:nr_rows(glist)) {
    gres <- glist[i,]
    scat(av_state$log_level,3,"    ", gres$perc,"\t",gres$desc,"\n",sep='')
  }
}

model_statistics <- function(lst,param) {
  tbl <- table(sapply(lst, 
                      function(x) ifelse(is.null(x$parameters[[param]]),
                                         FALSE,
                                         x$parameters[[param]])))
  tdescs <- names(tbl)
  tfreq <- sum(tbl)
  descs <- NULL
  freqs <- NULL
  percs <- NULL
  for (i in 1:length(tbl)) {
    desc <- tdescs[[i]]
    freq <- tbl[[i]]
    perc <- format_as_percentage(freq/tfreq)
    descs <- c(descs, desc)
    freqs <- c(freqs, freq)
    percs <- c(percs, perc)
  }
  df <- data.frame(desc = descs, freq = freqs, perc = percs, 
                   stringsAsFactors = FALSE)
  df <- df[with(df, order(df$freq, decreasing = TRUE)),]
  df
}


# Estimating the total search space
search_space_used <- function(av_state) {
  nvars <- length(av_state$vars)
  nexo <- 1+av_state$exogenous_max_iterations
  dlags <- distinct_lags(c(av_state$accepted_models,av_state$rejected_models))
  nlags <- length(dlags)
  tot_models <- 2* 2* nlags * nexo^nvars
  searched_models <- length(av_state$accepted_models) + length(av_state$rejected_models)
  scat(av_state$log_level,3,'Tested ',searched_models,' of ',tot_models,' (',
      format_as_percentage(searched_models/tot_models),') of the combinatorial search space at the given lags (',paste(dlags,collapse=', '),').\n',sep='')
  invisible(av_state)
}
distinct_lags <- function(lst) {
  sort(unique(sapply(lst,function(x) x$parameters$lag)))
}

# Creating the total search space
create_total_model_queue <- function(av_state) {
  r <- list()
  lags <- distinct_lags(c(av_state$accepted_models,av_state$rejected_models))
  vars <- sort(av_state$vars)
  nvars <- length(vars)
  nexo <- 1+av_state$exogenous_max_iteration
  ncombs <- nexo^nvars
  for (restrict in c(FALSE,TRUE)) {
    for (apply_log_transform in c(FALSE,TRUE)) {
      for (lag in lags) {
        for (i in 1:ncombs) {
          combline <- sapply(1:nvars,function(x) determine_var_index(i,x,nexo))
          exogenous_variables <- exogenous_dataframe_for_combline(combline,vars)
          model <- list()
          if (apply_log_transform) { model$apply_log_transform <- TRUE }
          if (!is.null(exogenous_variables)) { model$exogenous_variables <- exogenous_variables }
          model$lag <-lag
          if (restrict) { model$restrict <- TRUE }
          class(model) <- 'var_model'
          r <- c(r,list(model))
        }
      }
    }
  }
  r
}
determine_var_index <- function(i,vari,nexo) {
  ((i-1)%/%(nexo^(vari-1)))%%nexo
}
exogenous_dataframe_for_combline <- function(combline,vars) {
  dfs <- NULL
  for (i in 1:length(combline)) {
    if (combline[[i]] != 0) {
      df <- data.frame(variable=vars[[i]],iteration=combline[[i]],stringsAsFactors=FALSE)
      if (is.null(dfs)) {
        dfs <- df
      } else {
        dfs <- rbind(dfs,df)
      }
    }
  }
  dfs
}


#' Print a list of accepted models after a call to var_main
#' 
#' This function prints the list of accepted models when \code{av_state} is the result of a call to \code{\link{var_main}}. The printed list of models is sorted by AIC+BIC score.
#' @param av_state an object of class \code{av_state} that was the result of a call to \code{\link{var_main}}
#' @examples
#' # av_state is the result of a call to var_main
#' print_accepted_models(av_state)
#' @export
print_accepted_models <- function(av_state) {
  print(av_state$accepted_models,av_state)
}

#' Print a list of rejected models after a call to var_main
#' 
#' This function prints the list of rejected models when \code{av_state} is the result of a call to \code{\link{var_main}}.
#' @param av_state an object of class \code{av_state} that was the result of a call to \code{\link{var_main}}
#' @examples
#' # av_state is the result of a call to var_main
#' print_rejected_models(av_state)
#' @export
print_rejected_models <- function(av_state) {
  print(av_state$rejected_models,av_state)
}


find_model <- function(av_state,...) {
  model <- list(...)
  if (!is.null(av_state$accepted_models)) {
    accepted_matches <- find_models(av_state$accepted_models,model)
    if (!is.null(accepted_matches)) {
      cat(length(accepted_matches)," matches in accepted models:\n",sep='')
      for (i in accepted_matches) {
        cat('[[',i,']]\n',sep='')
        print(av_state$accepted_models[[i]],av_state)
        cat('\n')
      }
    }
  }
  if (!is.null(av_state$rejected_models)) {
    rejected_matches <- find_models(av_state$rejected_models,model)
    if (!is.null(rejected_matches)) {
      cat('\n',length(rejected_matches)," matches in rejected models:\n",sep='')
      for (i in rejected_matches) {
        cat('[[',i,']]\n',sep='')
        print(av_state$rejected_models[[i]],av_state)
        cat('\n')
      }
    }
  }
  invisible(av_state)
}

find_models <- function(model_list,given_model) {
  i <- 0
  r <- NULL
  for (model in model_list) {
    i <- i+1
    if (model_matches(given_model,model$parameters)) {
      r <- c(r,i)
    }
  }
  r
}

model_matches <- function(given_model,model) {
  names <- names(given_model)
  model <- merge_lists(default_model_props(),model)
  i <- 0
  matching <- TRUE
  for (value in given_model) {
    i <- i+1
    name <- names[[i]]
    if(((is.null(model[[name]]) && !is.null(value)) || (is.null(value) && !is.null(model[[name]]))) ||
         (!is.null(dim(model[[name]])) && !is.null(dim(value)) && dim(model[[name]]) != dim(value)) ||
         any(model[[name]] != value)) {
      matching <- FALSE
      break
    }
  }
  matching
}

