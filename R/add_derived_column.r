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
    av_state$data[[i]][[name]] <<- operation(columns,data_frame,i)
  }
  av_state$last_warning <<- NULL
}

add_derived_column_sum <- function(columns,data_frame,subset) {
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
        if (is.null(av_state$last_warning) || av_state$last_warning != mywarn) {
          av_state$last_warning <<- mywarn
          cat(mywarn," (for subset ",subset,")\n",sep="")
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

add_derived_column_ln <- function(column,data_frame,subset) {
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
    cat("add_derived_column_ln: increasing all values of column",
        column,"by",inc,"for subset",subset,"\n")
    data_column <- data_column+inc
  }
  log(data_column)
}

add_derived_column_mtoh <- function(column,data_frame,subset) {
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
