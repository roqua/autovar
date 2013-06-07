#' Split up a data set into different subsets
#' 
#' This function splits up the initial data set into multiple sets based on their value for \code{id_field}. For example, if we have a data set with a field named \code{id}  that has values ranging from 11 to 15, calling \code{av_state <- group_by('id')} will set \code{av_state$data} to a list of five items. This list is ordered by the value of the id field. Then, we can use \code{av_state$data[[1]]} (or equivalently, \code{av_state$data[['11']]}) to retrieve the rows in the data set that have \code{id} 11. Likewise, use \code{av_state$data[[2]]} or \code{av_state$data[['12']]} for rows with \code{id} 12. This function can only be called once per \code{av_state}.
#' @param av_state an object of class \code{av_state}
#' @param id_field the name of a column in the data set
#' @return This function returns the modified \code{av_state} object.
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav",log_level=3)
#' print(av_state)
#' av_state <- group_by(av_state,'id')
#' print(av_state)
#' @export
group_by <- function(av_state,id_field) {
  assert_av_state(av_state)
  if (missing(id_field)) { stop("missing id_field parameter") }
  if (!is.null(av_state$group_by)) {
    stop("group_by can only be called once")
  }

  av_state$group_by <- id_field
  av_state$data <- split_up(getElement(av_state$data,names(av_state$data)[1]),id_field)
  
  scat(av_state$log_level,2,"group_by: identified ",length(av_state$data),
      " groups based on distinct values for attribute '",id_field,"'\n",sep="")

  av_state
}

split_up <- function(data_frame,id_field) {
  data <- list()
  indices <- sort(unique(getElement(data_frame,id_field)))
  for (i in indices) {
    data[[as.character(i)]] <- data_frame[which(getElement(data_frame,id_field) == i),]
  }
  if (any(is.na(unique(getElement(data_frame,id_field))))) {
    data[[length(data)+1]] <- data_frame[which(is.na(getElement(data_frame,id_field))),]
    names(data)[[length(data)]] <- "NA"
  }
  data
}
