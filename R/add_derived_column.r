# add_derived_column

add_derived_column <- function(name,columns,operation=c('SUM','LN','MINUTES_TO_HOURS')) {
  operation <- match.arg(operation)
  operation <- switch(operation,
    SUM = add_derived_column_sum,
    LN = add_derived_column_ln,
    MINUTES_TO_HOURS = add_derived_column_mtoh
  )
  i <- 0
  for (data_frame in av_state$data) {
    i <- i+1
    av_state$data[[i]][[name]] <<- operation(columns,data_frame)
  }
}

add_derived_column_sum <- function(columns,data_frame) {
  csum <- 0
  warnflag <- TRUE
  for (column in columns) {
    data_column <- data_frame[[column]]
    if (is.null(data_column)) {
      stop(paste("column",column,"does not exist"))
    }
    if (class(data_column) != 'numeric') {
      if (warnflag) {
        warnflag <- FALSE
        mywarn <- paste("column",column,"is not numeric: converting...")
        if (!is.null(av_state$last_warning) && av_state$last_warning != mywarn) {
          av_state$last_warning <<- mywarn
          warning(mywarn)
        }
      }
      data_column <- as.numeric(data_column) -1
    }
    # for sum, default value is 0
    data_column[is.na(data_column)] <- 0
    csum <- csum + data_column
  }
  csum
}

add_derived_column_ln <- function(column,data_frame) {
  data_column <- data_frame[[column]]
  if (is.null(data_column)) {
    stop(paste("column",column,"does not exist"))
  }
  if (class(data_column) != 'numeric') {
    stop(paste("column",column,"is not numeric"))
  }
  # for ln, default value is 1
  data_column[is.na(data_column)] <- 1
  log(data_column)
}

add_derived_column_mtoh <- function(column,data_frame) {
  data_column <- data_frame[[column]]
  if (is.null(data_column)) {
    stop(paste("column",column,"does not exist"))
  }
  if (class(data_column) != 'numeric') {
    stop(paste("column",column,"is not numeric"))
  }
  data_column <- data_column/60
  data_column
}
