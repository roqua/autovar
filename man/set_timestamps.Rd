\name{set_timestamps}
\alias{set_timestamps}
\title{Add dummy variables for weekdays and hours of the day}
\usage{
  set_timestamps(av_state, subset_id = 1,
    date_of_first_measurement, measurements_per_day = 1,
    log_level = 0, add_days_as_exogenous = TRUE,
    add_dayparts_as_exogenous = TRUE)
}
\arguments{
  \item{av_state}{an object of class \code{av_state}}

  \item{subset_id}{either an integer subset index or the
  the value for the \code{id_field} column that was used in
  the \code{group_by} function. The \code{subset_id}
  argument is required if the data set is grouped into
  multiple data sets (i.e., if the \code{\link{group_by}}
  function was used), in which case the function works on
  the specified data subset.}

  \item{date_of_first_measurement}{the date of the first
  measurement. This argument should be given in the format:
  \code{"yyyy-mm-dd"}, e.g., \code{"2004-03-28"}.}

  \item{measurements_per_day}{how many measurements were
  taken per day. This default is 1. It is assumed that
  every day has exactly this amount of measurements, and
  that the first measurement in the dataset was the first
  measurement on that day.}

  \item{log_level}{sets the minimum level of output that
  should be shown (a number between 0 and 3). A lower level
  means more verbosity. Specify a log_level of 3 to hide
  messages about the exogenous columns being added.}

  \item{add_days_as_exogenous}{adds days as exogenous dummy
  variables to VAR models.}

  \item{add_dayparts_as_exogenous}{adds day parts as
  exogenous dummy variables to VAR models.}
}
\value{
  This function returns the modified \code{av_state}
  object.
}
\description{
  This function adds dummy columns for weekdays (named
  \code{Sunday}, \code{Monday}, \code{Tuesday},
  \code{Wednesday}, \code{Thursday}, \code{Friday} and
  \code{Saturday}) and hours of the day to the given subset
  of the specified data set. These are used by
  \code{\link{var_main}} to find better models by removing
  cyclicity from the data set.
}
\examples{
av_state <- load_file("../data/input/RuwedataAngela.sav")
av_state <- set_timestamps(av_state,date_of_first_measurement="2010-04-14")
}

