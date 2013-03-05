\name{visualize}
\alias{visualize}
\title{Visualize columns of the data set}
\usage{
  visualize(av_state, columns, ...)
}
\arguments{
  \item{av_state}{an object of class \code{av_state}}

  \item{columns}{specifies the columns to be displayed.
  When given the name of a single column, the function
  behaves differently depending on the class of the column:
  \itemize{ \item If the class of the column is
  \code{factor}, the column is seen as a nominal column,
  and the following arguments are accepted:
  \code{visualize(column,type=c('PIE','BAR','DOT','LINE'),title="",...)}.
  All plots also accept the \code{xlab} argument, e.g.,
  \code{xlab='minuten'}. Furthermore, when the type is
  \code{'BAR'}, an additional argument \code{horiz} can be
  supplied (\code{horiz} is \code{FALSE} by default), which
  will draw horizontal bar charts instead of vertical ones.
  To show values over time rather than total values, the
  \code{'LINE'} type can be used. Example:
  \code{visualize('PHQ1')} (assuming \code{'PHQ1'} is a
  \code{factor} column). \item If the class of the column
  is \code{numeric}, the column is seen as a scale column,
  and the following arguments are accepted:
  \code{visualize(column,type=c('LINE','BOX'),title="",...)}.
  Furthermore, when the type is \code{'LINE'}, an
  additional argument \code{acc} can be supplied
  (\code{acc} is \code{FALSE} by default), which will plot
  lines of accumulated values rather than the individual
  values. Example:
  \code{visualize('minuten_sport',type='LINE',acc=TRUE)}
  (assuming \code{'minuten_sport'} is a \code{numeric}
  column). } When the \code{columns} argument is given a
  list of column names, the columns are either shown as
  multiple lines in a line plot (when \code{type='LINE'}),
  or the sums of the columns are displayed in the plots
  (for any of the other types). When given a list of column
  names as the \code{columns} argument, the function
  accepts the following arguments:
  \code{visualize(columns,labels=columns,type=c('LINE','PIE','BAR','DOT'),
  title="",...)}. The arguments of this function work much
  like the ones described above for individual
  \code{factor} columns. The added optional \code{labels}
  argument should be a list of the same length as the
  \code{columns} argument, specifying custom names for the
  columns. This argument is ignored when
  \code{type='LINE'}.}
}
\description{
  This function works with single or multiple columns. When
  given an array of multiple columns as \code{columns}
  argument, all nonnumeric columns are converted to numeric
  class in the plot. This function creates a combined plot
  with individual plots for each identified group (if the
  \code{\link{group_by}} was used) in the current data set.
  Any supplied arguments other than the ones described are
  passed on to the plotting functions.
}
\examples{
av_state <- load_file("../data/input/RuwedataAngela.sav")
av_state <- add_derived_column(av_state,'sum_minuten_licht',
              c('minuten_woonwerk','minuten_werk_licht',
                'minuten_licht_huishouden'))
av_state <- add_derived_column(av_state,'sum_minuten_zwaar',
              c('minuten_zwaarhuishouden','minuten_zwaar_werk'))
visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar',
          'minuten_vrijetijd','minuten_sport'),
          labels=c('licht werk','zwaar werk','vrije tijd','sport'),
          type='BAR',horiz=TRUE)
visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar',
          'minuten_vrijetijd','minuten_sport'),
          type='DOT',xlab='minuten')
visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar',
          'minuten_vrijetijd','minuten_sport'))
}

