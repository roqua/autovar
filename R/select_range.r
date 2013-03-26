#' Select a subset of rows of a data set to be retained
#' 
#' This function selects which rows of a data set should be included. Either the \code{begin} or the \code{end} argument need to be specified. The column does not need to be sorted for this function to work. Values are included if they are \code{>= begin} and \code{<= end}, if specified, for the specified column. Values that are \code{NA} are never removed by this function.
#' @param av_state an object of class \code{av_state}
#' @param subset_id either an integer subset index or the the value for the \code{id_field} column that was used in the \code{group_by} function. The \code{subset_id} argument is required if the data set is grouped into multiple data sets (i.e., if the \code{\link{group_by}} function was used), in which case the function works on the specified data subset.
#' @param column specifies which column the \code{begin} and \code{end} values should be taken over. This argument is optional, and if it is missing, \code{id_field} used in \code{\link{order_by}} will be used.
#' @param begin indicates which values should be included. Values that are \code{>= begin} are included. This argument is optional if \code{end} is specified.
#' @param end indicates which values should be included. Values that are \code{<= end} are included. This argument is optional if \code{begin} is specified.
#' @return This function returns the modified \code{av_state} object.
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav")
#' av_state <- select_range(av_state,'1',begin=20,end=40)
#' @export
select_range <- function(av_state,subset_id=1,column,begin,end) {
  assert_av_state(av_state)
  if (class(subset_id) == 'numeric' && !any(subset_id == 1:length(av_state$data))) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  data_frame <- av_state$data[[subset_id]]
  if (is.null(data_frame)) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  if (missing(column)) {
    if (is.null(av_state$order_by)) { stop("please supply a column argument") }
    column <- av_state$order_by
  }
  data_column <- data_frame[[column]]
  if (is.null(data_column)) { stop(paste("column does not exist:",column)) }
  if (class(data_column) != 'numeric') {
    stop(paste("column",column,"has to be numeric, but is",class(data_column)))
  }
  if (missing(begin) && missing(end)) {
    stop("either begin or end need to be specified")
  }
  condition <- NULL
  if (missing(begin)) {
    condition <- data_column <= end
  } else if (missing(end)) {
    condition <- data_column >= begin
  } else {
    condition <- data_column <= end & data_column >= begin
  }
  missing_before <- calc_missing(av_state$data[[subset_id]])
  rows_remaining <- length(which(condition))
  rows_cut <- dim(data_frame)[1] - rows_remaining
  if (rows_cut > 0) {
    scat(av_state$log_level,2,"select_range: for subset ",subset_id,", number of rows cut: ",
        rows_cut,", number of rows remaining: ",rows_remaining,"\n",sep='')
  }
  av_state$data[[subset_id]] <- data_frame[which(condition),]
  missing_after <- calc_missing(av_state$data[[subset_id]])
  if (missing_before != missing_after) {
    scat(av_state$log_level,2,paste("select_range: for subset ",subset_id,", missing values went from ",missing_before," to ",missing_after,"\n",sep=""))
  }
  av_state
}

# also used in order_by
calc_missing <- function(data_frame) {
  na_cnt <- sum(is.na(data_frame))
  tot_cnt <- length(is.na(data_frame))
  paste(round(100*na_cnt/tot_cnt,digits=2),"% (",na_cnt,")",sep="")
}
