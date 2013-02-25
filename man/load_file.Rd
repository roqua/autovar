\name{load_file}
\alias{load_file}
\title{Load a data set from a .sav or a .dta file}
\usage{
  load_file(filename, file_type = c("SPSS", "STATA"),
    log_level = 0)
}
\arguments{
  \item{filename}{path and filename of the data set to
  load. The path can be relative to the current working
  directory.}

  \item{file_type}{specifies whether the file contains
  STATA or SPSS data. This argument is optional. When not
  specified, it is determined from the file name, i.e.,
  .dta extensions are treated as STATA files and .sav
  extensions are treated as SPSS files.}

  \item{log_level}{sets the minimum level of output that
  should be shown (a number between 0 and 3). A lower level
  means more verbosity. Specify a log_level of 3 to hide
  messages about the data set that was loaded.}
}
\value{
  This function returns the newly created \code{av_state}
  object.
}
\description{
  This function prints the columns of the loaded data set.
  The abbreviation \code{(scl)} is used to denote scale
  (numeric) columns, and \code{(nom)} is used to denote
  nominal (factor) columns. The function returns an object
  of class \code{av_state}. Objects of this class are used
  throughout this package to store VAR data sets, models,
  and results.
}
\examples{
av_state <- load_file("../data/input/RuwedataAngela.sav")
print(av_state)
}

