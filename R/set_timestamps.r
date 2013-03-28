#' Add dummy variables for weekdays and hours of the day
#' 
#' This function adds dummy columns for weekdays (named \code{Sunday}, \code{Monday}, \code{Tuesday}, \code{Wednesday}, \code{Thursday}, \code{Friday} and \code{Saturday}) and hours of the day to the given subset of the specified data set. These are used by \code{\link{var_main}} to find better models by removing cyclicity from the data set.
#' @param av_state an object of class \code{av_state}
#' @param subset_id either an integer subset index or the the value for the \code{id_field} column that was used in the \code{group_by} function. The \code{subset_id} argument is required if the data set is grouped into multiple data sets (i.e., if the \code{\link{group_by}} function was used), in which case the function works on the specified data subset.
#' @param date_of_first_measurement the date of the first measurement. This argument should be given in the format: \code{"yyyy-mm-dd"}, e.g., \code{"2004-03-28"}.
#' @param measurements_per_day how many measurements were taken per day. This default is 1. It is assumed that every day has exactly this amount of measurements, and that the first measurement in the dataset was the first measurement on that day.
#' @return This function returns the modified \code{av_state} object.
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav")
#' av_state <- set_timestamps(av_state,date_of_first_measurement="2010-04-14")
#' @export
set_timestamps <- function(av_state,subset_id=1,date_of_first_measurement,measurements_per_day=1) {
  stop("this is a stub")
#   assert_av_state(av_state)
#   if (class(subset_id) == 'numeric' && !any(subset_id == 1:length(av_state$data))) {
#     stop(paste(subset_id,"does not identify a data set"))
#   }
#   data_frame <- av_state$data[[subset_id]]
#   if (is.null(data_frame)) {
#     stop(paste(subset_id,"does not identify a data set"))
#   }
#   added_columns <- add_day_columns_aux(from,length(data_frame[[1]]),by)
#   av_state$added_day_columns <- TRUE
#   av_state$data[[subset_id]] <- cbind(av_state$data[[subset_id]],added_columns)
#   av_state
}

add_day_columns_aux <- function(from,length_out,by) {
  weekdayidx <- weekdays(timeSequence(from=from,length.out=length_out,by=by))
  weekday_labels <- weekdays(as.Date(timeSequence(from = "2012-01-01", to = "2012-01-07", by = "day")))
  weekday_labels_en <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
  r <- NULL
  i <- 0
  for (weekday in weekday_labels) {
    i <- i+1
    weekday_en <- weekday_labels_en[[i]]
    rl <- as.numeric(weekdayidx == weekday)
    if (is.null(r)) {
      r <- data.frame(rl)
    } else {
      r <- cbind(r,rl)
    }
    names(r)[[length(names(r))]] <- weekday_en
  }
  r
}
