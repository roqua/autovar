# add_to_queue

add_to_queue <- function(queue,model) {
  if (list(model) %in% queue) {
    scat(2,paste("> Not queueing: model already in queue:",list(model)),"\n")
    queue
  } else {
    c(queue,list(model))
  }
}
