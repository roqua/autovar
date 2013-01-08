# add_to_queue

add_to_queue <- function(queue,model) {
  if (list(model) %in% queue) {
    warning(paste("model already in queue:",list(model)))
    queue
  } else {
    c(queue,list(model))
  }
}
