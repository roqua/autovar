\name{add_day_columns}
\alias{add_day_columns}
\title{Add dummy variables for the weekday names}
\usage{
  add_day_columns(av_state, subset_id = 1, from,
    by = "day")
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

  \item{from}{the date of the first measurement. This
  argument should be given in the format:
  \code{"yyyy-mm-dd"}, e.g., \code{"2004-03-28"}.}

  \item{by}{by how much the consecutive measurements
  differ. The default is \code{"day"}.}
}
\value{
  This function returns the modified \code{av_state}
  object.
}
\description{
  This function adds dummy columns for weekdays (named
  \code{Sunday}, \code{Monday}, \code{Tuesday},
  \code{Wednesday}, \code{Thursday}, \code{Friday} and
  \code{Saturday}) to the given subset of the loaded data
  set.
}
\examples{
av_state <- load_file("../data/input/RuwedataAngela.sav")
av_state <- add_day_columns(av_state,from="2010-04-14")
}

