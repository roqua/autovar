#' Adds a trend variable to a data set
#' 
#' This function adds a column to a dataset, running from 1 to the length of the data set. If the data set is segmented using the group_by function, a column is added for every subset.
#' 
#' @param av_state an object of class \code{av_state}
#' @param subset_id either an integer subset index or the the value for the \code{id_field} column that was used in the \code{group_by} function. The \code{subset_id} argument is required if the data set is grouped into multiple data sets (i.e., if the \code{\link{group_by}} function was used), in which case the function works on the specified data subset.
#' @param varname the name of the newly created indexing column. Will be postfixed with underscores if the column already exists
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav")
#' av_state <- add_trend(av_state)
#' @export
add_trend <- function(av_state, subset_id = 1, varname = 'index') {
  assert_av_state(av_state)
  if (class(subset_id) == 'numeric' && !any(subset_id == 1:length(av_state$data))) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  data_frame <- av_state$data[[subset_id]]
  if (is.null(data_frame)) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  nname <- varname
  while (!is.null(data_frame[[nname]])) {
    nname <- paste(nname,'_',sep='')
  }
  newcol <- 1:(dim(data_frame)[[1]])
  av_state$data[[subset_id]] <- cbind(data_frame,newcol)
  names(av_state$data[[subset_id]])[[length(names(av_state$data[[subset_id]]))]] <- nname
  av_state$trend_vars <- nname
  sq_name <- paste(nname,'2',sep='')
  av_state$data[[subset_id]][[sq_name]] <- newcol*newcol
  av_state
}
