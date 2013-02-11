# select_range

# select_range selects a subset of rows to be kept
select_range <- function(subset_id='multiple',column,begin,end) {
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
    scat(2,"select_range: for subset ",subset_id,", number of rows cut: ",
        rows_cut,", number of rows remaining: ",rows_remaining,"\n",sep='')
  }
  av_state$data[[subset_id]] <<- data_frame[which(condition),]
  missing_after <- calc_missing(av_state$data[[subset_id]])
  if (missing_before != missing_after) {
    scat(2,paste("select_range: for subset ",subset_id,", missing values went from ",missing_before," to ",missing_after,"\n",sep=""))
  }
}

# also used in order_by
calc_missing <- function(data_frame) {
  na_cnt <- sum(is.na(data_frame))
  tot_cnt <- length(is.na(data_frame))
  paste(round(100*na_cnt/tot_cnt,digits=2),"% (",na_cnt,")",sep="")
}
