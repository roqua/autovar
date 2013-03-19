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
                     exogenous_max_iterations=3,subset=1,log_level=av_state$log_level,
                     small=FALSE,include_model=NULL,exogenous_variables=NULL) {
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
  av_state$exogenous_variables <- exogenous_variables

  scat(av_state$log_level,3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  scat(av_state$log_level,3,"Starting VAR with variables: ",paste(vars,collapse=', '),
       ", for subset: ",subset,"\n",sep='')
  
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
  for (varname in av_state$exogenous_variables) {
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
  for (mvar in av_state$exogenous_variables) {
    if (class(av_state$data[[av_state$subset]][[mvar]]) != "numeric") {
      scat(av_state$log_level,2,"column",mvar,"is not numeric, converting...\n")
      tv <- as.numeric(av_state$data[[av_state$subset]][[mvar]])
      av_state$data[[av_state$subset]][[mvar]] <- tv-min(tv)
    }
  }
  
  default_model <- list()
  class(default_model) <- 'var_model'
  av_state$model_queue <- list(default_model)
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
  av_state$resids <- list()
  av_state$log_resids <- list()
  i <- 1
  model_cnt <- 0
  while (TRUE) {
    if (i > length(av_state$model_queue)) { break }
    # pop a model and process it
    model <- av_state$model_queue[[i]]
    ev_modelres <- evaluate_model(av_state,model,i)
    av_state <- ev_modelres$av_state
    model_evaluation <- ev_modelres$res
    if (!is.null(model_evaluation$varest)) {
      model_cnt <- model_cnt +1
    }
    if (model_evaluation$model_valid) {
      # model is valid
      accepted_model <- list(parameters=model,varest=model_evaluation$varest)
      class(accepted_model) <- 'var_modelres'
      av_state$accepted_models <- c(av_state$accepted_models,list(accepted_model))
    } else if (!is.null(model_evaluation$varest)) {
      # model was rejected
      rejected_model <- list(parameters=model,varest=model_evaluation$varest)
      class(rejected_model) <- 'var_modelres'
      av_state$rejected_models <- c(av_state$rejected_models,list(rejected_model))
    }
    i <- i+1
  }
  scat(av_state$log_level,3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  scat(av_state$log_level,3,"\nDone. Processed",model_cnt,"models, of which",
      length(av_state$accepted_models),
      ifelse(length(av_state$accepted_models) == 1,"was","were"),"valid.\n")
  print_granger_statistics(av_state)
  class(av_state$accepted_models) <- 'model_list'
  class(av_state$rejected_models) <- 'model_list'
  if (length(av_state$accepted_models) > 0) {
    scat(av_state$log_level,3,"\nThe valid models:\n")
    av_state$accepted_models <- sort_models(av_state$accepted_models)
    sprint(av_state$log_level,3,av_state$accepted_models,av_state)
  }
  av_state$log_level <- real_log_level
  av_state
}

print_granger_statistics <- function(av_state) {
  lst <- c(av_state$accepted_models,av_state$rejected_models)
  scat(av_state$log_level,3,"\nGranger causality summary of all",
       length(lst),"processed models:\n")
  glist <- vargranger_list(lst)
  for (i in 1:nr_rows(glist)) {
    gres <- glist[i,]
    if (gres$desc == '') { next }
    scat(av_state$log_level,3,"  ",gres$perc,"\t",gres$desc,"\n",sep='')
  }
}

set_varest_values <- function(av_state,varest) {
  varest$small <- av_state$small
  varest$significance <- av_state$significance
  varest$vars <- av_state$vars
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
