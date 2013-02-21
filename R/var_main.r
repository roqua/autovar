# runs the model queue
# sets parameters in av_state
var_main <- function(av_state,vars,lag_max=7,significance=0.05,
                     exogenous_max_iterations=3,subset=1,log_level=av_state$log_level,
                     include_restricted_models=FALSE,small=FALSE,include_model=NULL) {
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
  av_state$include_restricted_models <- include_restricted_models
  av_state$small <- small

  scat(av_state$log_level,3,"\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  scat(av_state$log_level,3,"Starting VAR with variables: ",paste(vars,collapse=', '),
       ", for subset: ",subset,"\n",sep='')
  
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
      av_state$accepted_models <- add_to_queue(av_state$accepted_models,
                                                accepted_model,av_state$log_level)
    } else if (!is.null(model_evaluation$varest)) {
      # model was rejected
      rejected_model <- list(parameters=model,varest=model_evaluation$varest)
      class(rejected_model) <- 'var_modelres'
      av_state$rejected_models <- add_to_queue(av_state$rejected_models,
                                                rejected_model,av_state$log_level)
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

print_accepted_models <- function(av_state) {
  print(av_state$accepted_models,av_state)
}
print_rejected_models <- function(av_state) {
  print(av_state$rejected_models,av_state)
}
