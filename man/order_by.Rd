\name{order_by}
\alias{order_by}
\title{Order the rows in a data set}
\usage{
  order_by(av_state, id_field,
    impute_method = c("BEST_FIT", "ONE_MISSING", "ADD_MISSING", "NONE"),
    use_as_exogenous = FALSE)
}
\arguments{
  \item{av_state}{an object of class \code{av_state}}

  \item{id_field}{the name of a column in the data set}

  \item{impute_method}{this argument has four possible
  values: \itemize{ \item \code{'BEST_FIT'} - This is not
  an impute method itself, but tells the function to
  determine the optimal impute method and use that. This is
  the default choice for \code{impute_method} when it is
  not specified. \item \code{'ONE_MISSING'} - Only works
  when the \code{id_field} in each data subset is an
  integer range with exactly one value missing and exactly
  one \code{NA} value. The \code{NA} value is then
  substituted by the missing index. \item
  \code{'ADD_MISSING'} - Does not work when one or more
  rows have an \code{NA} value for \code{id_field}. Only
  works for integer ranges of \code{id_field} with single
  increments. Works by adding rows for all missing values
  in the range between the minimum and maximum value of
  \code{id_field}. All values in the added rows are
  \code{NA} except for the \code{id_field} and the field
  used for grouping the data (if there was one). \item
  \code{'NONE'} - No imputation is performed. }}

  \item{use_as_exogenous}{determines whether the
  \code{order_by} column and its squared values should be
  used as exogenous variables in VAR models.}
}
\value{
  This function returns the modified \code{av_state}
  object. After the substitutions, the data sets in
  \code{av_state$data} are sorted by their \code{id_field}
  value. This sorting step moves any rows with value
  \code{NA} for the \code{id_field} to the end.
}
\description{
  This function determines the order of the data rows in
  the data set. For vector autoregression, you may want to
  use this to make sure that the data set is sorted by the
  date/time column, i.e., the supplied \code{id_field}
  parameter is often a measurement index (e.g.,
  \code{'tijdstip'}). The \code{id_field} column has to be
  numeric. This function will also add a squared column to
  the data frame and include the order_by colum and its
  squared values as exogenous_variables.
}
\examples{
av_state <- load_file("../data/input/RuwedataAngela.sav")
av_state <- order_by(av_state,'tijdstip',impute_method='ONE_MISSING')
}

