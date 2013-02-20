add_to_queue <- function(queue,model,log_level=0) {
  if (list(model) %in% queue) {
    scat(log_level,2,paste("> Not queueing: model already in queue:",list(model)),"\n")
    queue
  } else {
    c(queue,list(model))
  }
}
