# group_by

# group_by splits the data
group_by <- function(id_field) {
  if (missing(id_field)) { stop("missing id_field parameter") }
  
  av_state$group_by <<- id_field
  av_state$data <<- split_up(getElement(av_state$data,names(av_state$data)[1]),id_field)
  
  cat("group_by: identified ",length(av_state$data),
      " groups based on distinct values for attribute '",id_field,"'\n",sep="")
}

split_up <- function(data_frame,id_field) {
  data <- list()
  indices <- sort(unique(getElement(data_frame,id_field)))
  for (i in indices) {
    data[[as.character(i)]] <- data_frame[getElement(data_frame,id_field) == i,]
  }
  data
}
