# var_main

# runs the model queue
# sets global parameters
var_main <- function(vars,lag_max=14,significance=0.05,exogenous_max_iterations=3,subset=1) {
  # lag_max is the global maximum lags used
  # significance is the limit
  # exogenous_max_iterations is the maximum number individual outliers that can be removed
  # subset is the chosen subset of data
  cat("\n",paste(rep('=',times=20),collapse=''),"\n",sep='')
  cat("Starting VAR with variables: ",vars,", for subset: ",subset,"\n",sep='')
  
  av_state$significance <<- significance  
  av_state$lag_max <<- lag_max
  av_state$exogenous_max_iterations <<- exogenous_max_iterations
  av_state$vars <<- vars
  av_state$subset <<- subset
  
  # check if subset exists
  if (is.null(av_state$data[[av_state$subset]])) {
    stop(paste("invalid subset specified:",av_state$subset))
  }
  
  # check if endoegnous columns exist
  if (is.null(av_state$data[[av_state$subset]][av_state$vars])) {
    stop(paste("invalid endogenous columns specified:",av_state$vars))
  }
  
  av_state$model_queue <<- list(list())
  av_state$accepted_models <<- list()
  i <- 1
  while (TRUE) {
    if (i > length(av_state$model_queue)) { break }
    # do stuff
    model <- av_state$model_queue[[i]]
    model_evaluation <- evaluate_model(model)
    if (model_evaluation$model_valid) {
      # model is valid
      accepted_model <- list(parameters=model,varest=model_evaluation$varest)
      av_state$accepted_models <<- add_to_queue(av_state$accepted_models,
                                                accepted_model)
    }
    i <- i+1
  }
  cat("Done. Processed",i-1,"models\n")
  av_state$accepted_models <<- sort_models(av_state$accepted_models)
  print(av_state$accepted_models)
}
