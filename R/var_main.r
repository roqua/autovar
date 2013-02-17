# var_main

# runs the model queue
# sets global parameters
var_main <- function(vars,lag_max=14,significance=0.05,
                     exogenous_max_iterations=3,subset=1,log_level=av_state$log_level,
                     include_restricted_models=FALSE,small=FALSE,include_model=NULL) {
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
  scat(3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  scat(3,"Starting VAR with variables: ",paste(vars,collapse=', '),
       ", for subset: ",subset,"\n",sep='')
  
  av_state$significance <<- significance  
  av_state$lag_max <<- lag_max
  av_state$exogenous_max_iterations <<- exogenous_max_iterations
  av_state$vars <<- vars
  av_state$subset <<- subset
  av_state$log_level <<- log_level
  av_state$include_restricted_models <<- include_restricted_models
  av_state$small <<- small

  # check if subset exists
  if (is.null(av_state$data[[av_state$subset]])) {
    stop(paste("invalid subset specified:",av_state$subset))
  }
  
  # check if endogenous columns exist
  if (is.null(av_state$data[[av_state$subset]][av_state$vars])) {
    stop(paste("invalid endogenous columns specified:",av_state$vars))
  }
  default_model <- list()
  class(default_model) <- 'var_model'
  av_state$model_queue <<- list(default_model)
  if (!is.null(include_model)) {
    if (class(include_model) != 'list') {
      stop(paste("the include_model object has to be of class list"))
    }
    new_model <- merge_lists(default_model,include_model)
    class(new_model) <- "var_model"
    av_state$model_queue <<- add_to_queue(av_state$model_queue,new_model)
  }
  av_state$accepted_models <<- list()
  av_state$rejected_models <<- list()
  i <- 1
  model_cnt <- 0
  while (TRUE) {
    if (i > length(av_state$model_queue)) { break }
    # pop a model and process it
    model <- av_state$model_queue[[i]]
    model_evaluation <- evaluate_model(model,i)
    if (!is.null(model_evaluation$varest)) {
      model_cnt <- model_cnt +1
    }
    if (model_evaluation$model_valid) {
      # model is valid
      accepted_model <- list(parameters=model,varest=model_evaluation$varest)
      class(accepted_model) <- 'var_modelres'
      av_state$accepted_models <<- add_to_queue(av_state$accepted_models,
                                                accepted_model)
    } else if (!is.null(model_evaluation$varest)) {
      # model was rejected
      rejected_model <- list(parameters=model,varest=model_evaluation$varest)
      class(rejected_model) <- 'var_modelres'
      av_state$rejected_models <<- add_to_queue(av_state$rejected_models,
                                                rejected_model)
    }
    i <- i+1
  }
  scat(3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  scat(3,"\nDone. Processed",model_cnt,"models, of which",
      length(av_state$accepted_models),
      "were valid:\n")
  av_state$accepted_models <<- sort_models(av_state$accepted_models)
  sprint(3,av_state$accepted_models)
  av_state$log_level <<- real_log_level
}
