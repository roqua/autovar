#' Add a new column using data from other columns
#' 
#' This function adds a new column, based on existing columns, to all identified groups in the given data set.
#' @param av_state an object of class \code{av_state}
#' @param name the name of the new column
#' @param columns the existing columns that the new column will be based on
#' @param operation this argument has three possible values: \itemize{
#' \item \code{'SUM'} - The new column is the sum of the columns specified in the columns argument. So for this option, the columns argument is an array of column names. Values in the summation of columns that are NA are treated as if they're zero. Columns that are not numeric are transformed to numeric. For example, Factor columns are transformed to numbers starting at 0 for the first factor level.
#' \item \code{'AVG'} - The new column is the average of the columns specified in the columns argument. For each row, the resulting column has the average value of all columns that are not NA on that row, or NA otherwise.
#' \item \code{'LN'} - The new column is the natural logarithm of the specified column in columns. Thus, for this option, the columns argument is simply the name of a single column. This operation does not work on columns that are not numeric. Values in the original column that are NA are left as NA in the new column. Note that values are increased if necessary so that the resulting column has no negative values.
#' \item \code{'MINUTES_TO_HOURS'} - The new column is the values of the specified column divided by 60. Thus, for this option, the columns argument is simply the name of a single column. This operation does not work on columns that are not numeric. Values in the original column that are NA are left as NA in the new column.
#' \item \code{'SQUARED'} - The new column ise the square of the values of the specified column. Thus, for this option, the columns argument is simply the name of a single column. This operation does not work on columns that are not numeric. Values in the original column that are NA are left as NA in the new column.
#' }
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity. Specify a log_level of 3 to hide messages about converting columns or increasing values for the 'LN' option.
#' @return This function returns the modified \code{av_state} object.
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav")
#' av_state <- add_derived_column(av_state,'SomPHQ',c('PHQ1','PHQ2','PHQ3','PHQ4',
#'                                'PHQ5','PHQ6','PHQ7','PHQ8','PHQ9'),
#'                                operation='SUM')
#' column_names_output(av_state)
#' av_state <- load_file("../data/input/pp1 nieuw compleet.sav",log_level=3)
#' av_state <- add_derived_column(av_state,'SomBewegUur','SomBewegen',
#'                                operation='MINUTES_TO_HOURS')
#' av_state <- add_derived_column(av_state,'lnSomBewegUur','SomBewegUur',
#'                                operation='LN')
#' av_state$data[[1]][c('SomBewegen','SomBewegUur','lnSomBewegUur')]
#' @export
add_derived_column <- function(av_state,name,columns,operation=c('SUM','AVG','LN','MINUTES_TO_HOURS','SQUARED'),log_level=0) {
  assert_av_state(av_state)
  operation <- match.arg(operation)
  operation <- switch(operation,
    SUM = add_derived_column_sum,
    AVG = add_derived_column_avg,
    LN = add_derived_column_ln,
    MINUTES_TO_HOURS = add_derived_column_mtoh,
    SQUARED = add_derived_column_squared
  )
  i <- 0
  for (data_frame in av_state$data) {
    i <- i+1
    av_state$data[[i]][[name]] <- operation(columns,data_frame,i,log_level)
  }
  # av_state$last_warning <- NULL
  av_state
}

add_derived_column_sum <- function(columns,data_frame,subset,log_level) {
  csum <- 0
  warnflag <- TRUE
  for (column in columns) {
    data_column <- data_frame[[column]]
    if (is.null(data_column)) {
      stop(paste("column",column,"does not exist for subset",subset))
    }
    if (class(data_column) != 'numeric') {
      if (warnflag) {
        warnflag <- FALSE
        mywarn <- paste("column",column,"is not numeric: converting...")
        #if (is.null(av_state$last_warning) || av_state$last_warning != mywarn) {
          #av_state$last_warning <- mywarn
          scat(log_level,2,mywarn," (for subset ",subset,")\n",sep="")
        #}
      }
      data_column <- as.numeric(data_column) -1
    }
    # for sum, default value is 0
    data_column[is.na(data_column)] <- 0
    csum <- csum + data_column
  }
  csum
}

add_derived_column_avg <- function(columns,data_frame,subset,log_level) {
  csum <- NULL
  for (column in columns) {
    data_column <- data_frame[[column]]
    if (is.null(data_column))
      stop(paste("column",column,"does not exist for subset",subset))
  }
  for (i in 1:(dim(data_frame)[1])) {
    if (i > dim(data_frame)[1]) break;
    sum_val <- 0
    sum_count <- 0
    for (column in columns) {
      col_val <- data_frame[[column]][i]
      if (is.na(col_val)) next;
      col_val <- as.numeric(col_val)
      sum_val <- sum_val + col_val
      sum_count <- sum_count + 1
    }
    if (sum_count > 0) {
      csum <- c(csum,sum_val/sum_count)
    } else {
      csum <- c(csum,NA)
    }
  }
  csum
}

add_derived_column_ln <- function(column,data_frame,subset,log_level) {
  data_column <- data_frame[[column]]
  if (is.null(data_column)) {
    stop(paste("column",column,"does not exist for subset",subset))
  }
  if (class(data_column) != 'numeric') {
    stop(paste("column",column,"is not numeric for subset",subset))
  }
  # for ln, default value is 1
  # don't do this here, we might want to impute these later
  #data_column[is.na(data_column)] <- 1
  
  # scale minimum value to 
  inc <- 1-min(data_column,na.rm=TRUE)
  if (inc > 0) {
    scat(log_level,2,"add_derived_column_ln: increasing all values of column",
        column,"by",inc,"for subset",subset,"\n")
    data_column <- data_column+inc
  }
  log(data_column)
}

add_derived_column_mtoh <- function(column,data_frame,subset,log_level) {
  data_column <- data_frame[[column]]
  if (is.null(data_column)) {
    stop(paste("column",column,"does not exist for subset",subset))
  }
  if (class(data_column) != 'numeric') {
    stop(paste("column",column,"is not numeric for subset",subset))
  }
  data_column <- data_column/60
  data_column
}

add_derived_column_squared <- function(column,data_frame,subset,log_level) {
  data_column <- data_frame[[column]]
  if (is.null(data_column)) {
    stop(paste("column",column,"does not exist for subset",subset))
  }
  if (class(data_column) != 'numeric') {
    stop(paste("column",column,"is not numeric for subset",subset))
  }
  data_column <- data_column*data_column
  data_column
}
