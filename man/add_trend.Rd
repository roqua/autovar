\name{add_trend}
\alias{add_trend}
\title{Adds a trend variable to a data set}
\usage{
  add_trend(av_state, subset_id = 1, varname = "index")
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

  \item{varname}{the name of the newly created indexing
  column. Will be postfixed with underscores if the column
  already exists}
}
\description{
  This function adds a column to a dataset, running from 1
  to the length of the data set. If the data set is
  segmented using the group_by function, a column is added
  for every subset.
}
\examples{
av_state <- load_file("../data/input/RuwedataAngela.sav")
av_state <- add_trend(av_state)
}

