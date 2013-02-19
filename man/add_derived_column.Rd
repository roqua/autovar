\name{add_derived_column}
\alias{add_derived_column}
\title{Add a new column using data from other columns}
\usage{
  add_derived_column(name, columns,
    operation = c("SUM", "LN", "MINUTES_TO_HOURS"))
}
\arguments{
  \item{name}{the name of the new column}

  \item{columns}{the existing columns that the new column
  will be based on}

  \item{operation}{this argument has three possible values:
  \itemize{ \item 'SUM' - The new column is the sum of the
  columns specified in the columns argument. So for this
  option, the columns argument is an array of column names.
  Values in the summation of columns that are NA are
  treated as if they're zero. Columns that are not numeric
  are transformed to numeric. For example, Factor columns
  are transformed to numbers starting at 0 for the first
  factor level. \item 'LN' - The new column is the natural
  logarithm of the specified column in columns. Thus, for
  this option, the columns argument is simply the name of a
  single column. This operation does not work on columns
  that are not numeric. Values in the original column that
  are NA are left as NA in the new column. Note that values
  are increased if necessary so that the resulting column
  has no negative values. \item 'MINUTES_TO_HOURS' - The
  new column is the values of the specified column divided
  by 60. Thus, for this option, the columns argument is
  simply the name of a single column. This operation does
  nto work on columns that are not numeric. Values in the
  original column that are NA are left as NA in the new
  column. }}
}
\description{
  This function adds a new column, based on existing
  columns, to all identified groups in the given data set.
}
\examples{
add_derived_column('SomPHQ',c('PHQ1','PHQ2','PHQ3','PHQ4',
                   'PHQ5','PHQ6','PHQ7','PHQ8','PHQ9'),
                   operation='SUM')
add_derived_column('lnSomBewegUur','SomBewegUur',
                   operation='LN')
add_derived_column('SomBewegUur','SomBewegen',
                   operation='MINUTES_TO_HOURS')
}

