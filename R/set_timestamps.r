#' Add dummy variables for weekdays and day parts
#' 
#' This function adds dummy columns for weekdays (named \code{Sunday}, \code{Monday}, \code{Tuesday}, \code{Wednesday}, \code{Thursday}, \code{Friday} and \code{Saturday}) and day parts (\code{morning}, \code{afternoon}) to the given subset of the specified data set. These are used by \code{\link{var_main}} to find better models by removing cyclicity from the data set.
#' @param av_state an object of class \code{av_state}
#' @param subset_id either an integer subset index or the the value for the \code{id_field} column that was used in the \code{\link{group_by}} function. The \code{subset_id} argument is required if the data set is grouped into multiple data sets (i.e., if the \code{\link{group_by}} function was used), in which case the function works on the specified data subset.
#' @param date_of_first_measurement the date of the first measurement. This argument should be given in the format: \code{"yyyy-mm-dd"}, e.g., \code{"2004-03-28"}.
#' @param measurements_per_day how many measurements were taken per day. This default is 1. It is assumed that every day has exactly this amount of measurements, and that the first measurement in the dataset was the first measurement on that day.
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity. Specify a log_level of 3 to hide messages about the exogenous columns being added.
#' @param first_measurement_index is used to specify that the first day of measurements has fewer than \code{measurements_per_day} measurements. Here, we assume that the measurements in the data set still form a connected sequence. In other words, the assumption is that the missing measurements of the first day precede the first measurement in the data set. For example, by specifying \code{measurements_per_day = 3, first_measurement_index = 2}, the first measurement in the data set will be treated as the second measurement of that day. So the first two measurements in the data set will be tagged with \code{Afternoon} and \code{Evening}, and the third measurement in the data set will be tagged with \code{Morning} of the next day.
#' @param add_days_as_exogenous adds days as exogenous dummy variables to VAR models.
#' @param add_dayparts_as_exogenous adds day parts as exogenous dummy variables to VAR models.
#' @param add_weekend_as_exogenous adds one exogenous variable named \code{Weekend} to the VAR models. This variable is 1 for weekend days (Saturday and Sunday) and 0 otherwise. By specifying \code{add_days_as_exogenous = FALSE} and \code{add_weekend_as_exogenous = TRUE}, the weekend is used instead of day dummies in the evaluation of models.
#' @return This function returns the modified \code{av_state} object.
#' @examples
#' av_state <- load_file("../data/input/pp4 nieuw compleet met 140min.sav",log_level=3)
#' av_state <- set_timestamps(av_state,date_of_first_measurement="2010-04-14")
#' 
#' # an example with multiple measurements per day:
#' av_state <- load_file("../data/input/ID68 basisbestand.sav",log_level=3)
#' av_state <- set_timestamps(av_state,date_of_first_measurement="2012-07-12",
#'                            measurements_per_day=3)
#' 
#' # same data set, but one extra day because the first day only has one measurement
#' av_state <- load_file("../data/input/ID68 basisbestand.sav",log_level=3)
#' av_state <- set_timestamps(av_state,date_of_first_measurement="2012-07-12",
#'                            measurements_per_day=3,first_measurement_index=3)
#' @export
set_timestamps <- function(av_state,subset_id=1,date_of_first_measurement,
                           measurements_per_day=1,log_level=0,
                           first_measurement_index=1,
                           add_days_as_exogenous=TRUE,
                           add_dayparts_as_exogenous=TRUE,
                           add_weekend_as_exogenous=FALSE) {
  assert_av_state(av_state)
  if (class(subset_id) == 'numeric' && !any(subset_id == 1:length(av_state$data))) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  data_frame <- av_state$data[[subset_id]]
  if (is.null(data_frame)) {
    stop(paste(subset_id,"does not identify a data set"))
  }
  first_measurement_index <- trunc(first_measurement_index)
  if (first_measurement_index != 1) {
    if (first_measurement_index < 1 || first_measurement_index > measurements_per_day) {
      stop(paste("first_measurement_index",first_measurement_index,"is invalid"))
    }
  }
  from <- timeDate::timeDate(as.Date(timeDate::timeDate(date_of_first_measurement)))
  column_info <- set_timestamps_aux(from,length(data_frame[[1]]),
                                    measurements_per_day,
                                    first_measurement_index,
                                    add_days_as_exogenous,
                                    add_dayparts_as_exogenous,
                                    add_weekend_as_exogenous)
  added_columns <- column_info$columns
  exovrs_d <- column_info$exovrs_d
  exovrs_h <- column_info$exovrs_h
  exovrs_w <- column_info$exovrs_w
  signif_columns_d <- NULL
  av_state$day_dummies <- NULL

  if (!is.null(exovrs_d)) {
    signif_columns_d <- significant_columns(added_columns,exovrs_d)
    if (!is.null(signif_columns_d)) {
      av_state$day_dummies <- c(av_state$day_dummies, signif_columns_d)
    }
  }
  signif_columns_w <- NULL
  if (!is.null(exovrs_w)) {
    signif_columns_w <- 'Weekend'
    av_state$day_dummies <- c(av_state$day_dummies, signif_columns_w)
  }
  signif_columns_h <- NULL
  if (!is.null(exovrs_h)) {
    signif_columns_h <- significant_columns(added_columns,exovrs_h)
    if (!is.null(signif_columns_h)) {
      av_state <- add_exogenous_variables(av_state,signif_columns_h)
    }
  }
  av_state$data[[subset_id]] <- cbind(av_state$data[[subset_id]],added_columns)
  signif_columns <- c(signif_columns_d,signif_columns_h,signif_columns_w)
  tsspan <- timeDate::timeSequence(from=next_day(from),
               length.out=ceiling((length(data_frame[[1]])-(measurements_per_day-first_measurement_index+1))/measurements_per_day),
               by="day")
  scat(log_level,2,"set_timestamps: dates range from ",
       as.character(as.character(from)),
       ' to ',
       as.character(tsspan[length(tsspan)]),
       '\n',sep='')
  if (!is.null(signif_columns)) {
    scat(log_level,2,"set_timestamps: using additional exogenous variables in models:\n   ",
         paste(signif_columns,collapse=", "),"\n")
  }
  av_state
}

next_day <- function(daystr) {
  as.character(timeDate::timeSequence(from=timeDate::timeDate(as.Date(timeDate::timeDate(daystr))),
                            length.out=2,by="day")[2])
}

set_timestamps_aux <- function(from,length_out,measurements_per_day,
                               first_measurement_index,add_days_as_exogenous,
                               add_dayparts_as_exogenous,add_weekend_as_exogenous) {
  firstday <- rep(weekdays(as.Date(timeDate::timeSequence(from=from,length.out=1,by="day")), FALSE),
                  each=measurements_per_day-first_measurement_index + 1)
  restofdays <- rep(weekdays(as.Date(timeDate::timeSequence(from=next_day(from),
                                          length.out=ceiling(length_out/measurements_per_day),
                                          by="day")), FALSE),each=measurements_per_day)
  weekdayidx <- c(firstday,restofdays)[1:length_out]
  weekday_labels <- weekdays(as.Date(timeDate::timeSequence(from = "2012-01-01",
                                                  to = "2012-01-07",
                                                  by = "day")), FALSE)
  weekday_labels_en <- c('Sunday','Monday','Tuesday','Wednesday',
                         'Thursday','Friday','Saturday')
  r <- NULL
  i <- 0
  exovrs_d <- NULL
  if (add_days_as_exogenous) {
    exovrs_d <- weekday_labels_en
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
  
  exovrs_w <- NULL
  weekend_indices <- weekday_labels[c(1,7)]
  weekend_column <- as.numeric(weekdayidx %in% weekend_indices)
  if (length(unique(weekend_column)) > 1) {
    if (add_weekend_as_exogenous) {
      exovrs_w <- 'Weekend'
    }
    if (is.null(r)) {
      r <- data.frame(weekend_column)
    } else {
      r <- cbind(r,weekend_column)
    }
    names(r)[[length(names(r))]] <- 'Weekend'
  }

  hfirstday <- daypart_string(measurements_per_day)[first_measurement_index:measurements_per_day]
  hrestofdays <- rep(daypart_string(measurements_per_day),
                     times=ceiling(length_out/measurements_per_day))
  houridx <- c(hfirstday,hrestofdays)[1:length_out]
  hour_columns <- daypart_string(measurements_per_day)
  exovrs_h <- NULL
  if (length(unique(houridx)) > 1) {
    if (add_dayparts_as_exogenous) {
      exovrs_h <- hour_columns
    }
    # multiple measurements per day, add daypart columns to the dataset
    for (hour_column in hour_columns) {
      rl <- as.numeric(houridx == hour_column)
      if (is.null(r)) {
        r <- data.frame(rl)
      } else {
        r <- cbind(r,rl)
      }
      names(r)[[length(names(r))]] <- hour_column
    }
  }
  list(columns=r,exovrs_d=exovrs_d,exovrs_h=exovrs_h,exovrs_w=exovrs_w)
}

daypart_string <- function(len) {
  r <- NULL
  if (len == 3) {
    r <- c('Morning','Afternoon','Evening')
  } else {
    for (i in 1:len) {
      if (i > len) { break }
      r <- c(r,paste('dailymeas_',i,'_of_',len,sep=''))
    }
  }
  r
}

significant_columns <- function(dataframe,names) {
  rnames <- NULL
  for (name in names) {
    column <- dataframe[[name]]
    # can be 0 if column doesnt exist in dataframe
    if (length(unique(column)) > 1) {
      rnames <- c(rnames,name)
    }
  }
  # if the data set includes all day parts or all days,
  # then one of them is not necessary, so remove it.
  if (length(names) == length(rnames)) {
    remove_last(rnames)
  } else {
    rnames
  }
}

remove_last <- function(lst) {
  lst[which(lst != lst[[length(lst)]])]
}
