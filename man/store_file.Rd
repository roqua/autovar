\name{store_file}
\alias{store_file}
\title{Export a modified data set as an SPSS readable .sas file}
\usage{
  store_file(av_state, filename, inline_data,
    file_type = c("SPSS", "STATA"))
}
\arguments{
  \item{av_state}{an object of class \code{av_state}}

  \item{filename}{the name that the exported files should
  be saved under. Do not include a path. When this argument
  is missing, the filename of the input file (used in
  \code{\link{load_file}}) is substituted.}

  \item{inline_data}{boolean argument to determine whether
  data should be stored inline in the .sas script. This
  argument is optional, and defaults to \code{TRUE} if the
  data set has less than 80 columns, and to \code{FALSE}
  otherwise.}

  \item{file_type}{sets the type of file export that should
  be used. Currently, only \code{'SPSS'} is supported.}
}
\description{
  This function exports all subsets in the specified
  \code{av_state} to individual output files. All output
  files are subsequently packed in a .tar file. Data sets
  with less than 80 columns are stored inline in the .sas
  script, otherwise, the data is stored in separate files.
}
\examples{
av_state <- load_file("../data/input/pp5.dta")
av_state <- group_by(av_state,'id')
av_state <- order_by(av_state,'Day')
av_state <- add_derived_column(av_state,'Activity_hours','Activity',
                               operation='MINUTES_TO_HOURS')
store_file(av_state)
}

