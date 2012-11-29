# add_derived_column

add_derived_column <- function(name,columns,operation=c('SUM')) {
  operation <- match.arg(operation)
  operation <- switch(operation,
    SUM = add_derived_column_sum
  )
  i <- 0
  for (data_frame in av_state$data) {
    i <- i+1
    av_state$data[[i]][[name]] <<- operation(columns,data_frame)
  }
}

add_derived_column_sum <- function(columns,data_frame) {
  csum <- 0
  for (column in columns) {
    data_column <- data_frame[[column]]
    # for sum, default value is 0
    data_column[is.na(data_column)] <- 0
    csum <- csum + data_column
  }
  csum
}
