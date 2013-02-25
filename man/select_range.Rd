\name{select_range}
\alias{select_range}
\title{Select a subset of rows of a data set to be retained}
\usage{
  select_range(av_state, subset_id = "multiple", column,
    begin, end)
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

  \item{column}{specifies which column the \code{begin} and
  \code{end} values should be taken over. This argument is
  optional, and if it is missing, \code{id_field} used in
  \code{\link{order_by}} will be used.}

  \item{begin}{indicates which values should be included.
  Values that are \code{>= begin} are included. This
  argument is optional if \code{end} is specified.}

  \item{end}{indicates which values should be included.
  Values that are \code{<= end} are included. This argument
  is optional if \code{begin} is specified.}
}
\value{
  This function returns the modified \code{av_state}
  object.
}
\description{
  This function selects which rows of a data set should be
  included. Either the \code{begin} or the \code{end}
  argument need to be specified. The column does not need
  to be sorted for this function to work. Values are
  included if they are \code{>= begin} and \code{<= end},
  if specified, for the specified column. Values that are
  \code{NA} are never removed by this function.
}
\examples{
av_state <- load_file("../data/input/RuwedataAngela.sav")
av_state <- select_range(av_state,'1',begin=20,end=40)
}

