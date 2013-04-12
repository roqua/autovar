#' Add dummy variables for weekdays and hours of the day
#' 
#' This function adds dummy columns for weekdays (named \code{Sunday}, \code{Monday}, \code{Tuesday}, \code{Wednesday}, \code{Thursday}, \code{Friday} and \code{Saturday}) and hours of the day to the given subset of the specified data set. These are used by \code{\link{var_main}} to find better models by removing cyclicity from the data set.
#' @param av_state an object of class \code{av_state}
#' @param subset_id either an integer subset index or the the value for the \code{id_field} column that was used in the \code{group_by} function. The \code{subset_id} argument is required if the data set is grouped into multiple data sets (i.e., if the \code{\link{group_by}} function was used), in which case the function works on the specified data subset.
#' @param date_of_first_measurement the date of the first measurement. This argument should be given in the format: \code{"yyyy-mm-dd"}, e.g., \code{"2004-03-28"}.
#' @param measurements_per_day how many measurements were taken per day. This default is 1. It is assumed that every day has exactly this amount of measurements, and that the first measurement in the dataset was the first measurement on that day.
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity. Specify a log_level of 3 to hide messages about the exogenous columns being added.
#' @param add_days_as_exogenous adds days as exogenous dummy variables to VAR models.
#' @param add_dayparts_as_exogenous adds day parts as exogenous dummy variables to VAR models.
#' @return This function returns the modified \code{av_state} object.
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav")
#' av_state <- set_timestamps(av_state,date_of_first_measurement="2010-04-14")
#' @export
set_timestamps <- function(av_state,subset_id=1,date_of_first_measurement,
                           measurements_per_day=1,log_level=0,
                           add_days_as_exogenous=FALSE,
                           add_dayparts_as_exogenous=TRUE) {
  assert_av_state(av_state)
  if (class(subset_id) == 'numeric' && !any(subset_id == 1:length(av_state$data))) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  data_frame <- av_state$data[[subset_id]]
  if (is.null(data_frame)) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  if (24%%measurements_per_day != 0) {
    stop("measurements_per_day needs to be a divisor of 24")
  }
  by <- paste(24%/%measurements_per_day,"hours")
  from <- timeDate(as.Date(timeDate(date_of_first_measurement)))
  column_info <- set_timestamps_aux(from,length(data_frame[[1]]),by,
                                    add_days_as_exogenous,
                                    add_dayparts_as_exogenous)
  added_columns <- column_info$columns
  exovrs <- column_info$exovrs
  signif_columns <- significant_columns(added_columns,exovrs)
  av_state <- add_exogenous_variables(av_state,signif_columns)
  av_state$data[[subset_id]] <- cbind(av_state$data[[subset_id]],added_columns)
  if (!is.null(signif_columns)) {
    scat(log_level,2,"set_timestamps: using additional exogenous variables in models:\n   ",
         paste(signif_columns,collapse=", "),"\n")
  }
  av_state
}

set_timestamps_aux <- function(from,length_out,by,add_days_as_exogenous,add_dayparts_as_exogenous) {
  weekdayidx <- weekdays(timeSequence(from=from,length.out=length_out,by=by))
  weekday_labels <- weekdays(as.Date(timeSequence(from = "2012-01-01", to = "2012-01-07", by = "day")))
  weekday_labels_en <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
  r <- NULL
  i <- 0
  exovrs <- NULL
  if (add_days_as_exogenous) { 
    exovrs <- remove_last(weekday_labels_en)
  }
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
  
  houridx <- format(timeSequence(from=from,length.out=length_out,by=by),"%H")
  u_hours <- unique(houridx)
  hour_columns <- sapply(u_hours,function (x) paste('hour_',x,sep=''))
  if (length(u_hours) > 1) {
    if (add_dayparts_as_exogenous) {
      exovrs <- c(exovrs,remove_last(hour_columns))
    }
    r <- NULL
    # multiple measurements per day, add hours columns to the dataset
    i <- 0
    for (u_hour in u_hours) {
      i <- i+1
      hour_column <- hour_columns[[i]]
      rl <- as.numeric(houridx == u_hour)
      if (is.null(r)) {
        r <- data.frame(rl)
      } else {
        r <- cbind(r,rl)
      }
      names(r)[[length(names(r))]] <- hour_column
    }
  }
  list(columns=r,exovrs=exovrs)
}

significant_columns <- function(dataframe,names) {
  rnames <- NULL
  for (name in names) {
    column <- dataframe[[name]]
    if (length(unique(column)) != 1) {
      rnames <- c(rnames,name)
    }
  }
  rnames
}

remove_last <- function(lst) {
  lst[which(lst != lst[[length(lst)]])]
}
