# select_range

# select_range selects a subset of rows to be kept
select_range <- function(subset_id='multiple',column,begin,end) {
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
  av_state$data[[subset_id]] <<- data_frame[which(condition),]
  missing_after <- calc_missing(av_state$data[[subset_id]])
  if (missing_before != missing_after) {
    cat(paste("select_range: missing values went from",missing_before,"to",missing_after,"for subset",subset_id,"\n"))
  }
}

# also used in order_by
calc_missing <- function(data_frame) {
  na_cnt <- sum(is.na(data_frame))
  tot_cnt <- length(is.na(data_frame))
  paste(round(100*na_cnt/tot_cnt,digits=2),"% (",na_cnt,")",sep="")
}
