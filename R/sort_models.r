# sorts models by highest model_score first

sort_models <- function(model_list) {
  # each model has a parameters and a varest property
  #sapply()
  score_list <- sapply(av_state$accepted_models,function(x) model_score(x$varest))
  sortres <- sort(score_list,index.return=TRUE)
  model_list[sortres$ix]
}
