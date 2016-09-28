#' Impute missing values
#' 
#' This function can impute data for values that are missing (i.e., for values that are \code{NA}). It outputs for every subset how many values were imputed (for all columns total) along with a percentage (following \code{<=}). This percentage is the percentage of the column with the highest percentage of imputed values, i.e., if multiple columns were specified, it is the percentage of values that were imputed of the column that had relatively most \code{NA} values.
#' @param av_state an object of class \code{av_state}
#' @param columns the columns of which missing values should be imputed. This argument can be a single column or a vector of column names.
#' @param subset_ids identifies which data subsets to impute data for. This argument can be a single subset, a range of subsets (both of which are identified by their indices), or it can be the word \code{'ALL'} (default). In the latter case, the selected columns of all data subsets are processed.
#' @param type this argument has two possible values: \itemize{
#' \item \code{'SIMPLE'} - The value of the missing data is determined by up to five values surrounding the value (2 before, 3 after, unless at the start or end of the range). For numeric (scl) columns, the mean of these values is chosen as value. For factor (nom) columns, the mode of these values is chosen as value.
#' \item \code{'EM'} - Em imputation. Currently not implemented.
#' }
#' @return This function returns the modified \code{av_state} object.
#' @examples
#' \dontrun{
#' av_state <- load_file("../data/input/RuwedataAngela.sav",log_level=3)
#' av_state <- group_by(av_state,'id')
#' print(av_state)
#' av_statea <- impute_missing_values(av_state,'norm_bewegen')
#' print(av_statea)
#' av_stateb <- impute_missing_values(av_state,c('norm_bewegen',
#'                                    'minuten_woonwerk'),subset_ids=1)
#' print(av_stateb)
#' }
#' @export
impute_missing_values <- function(av_state,columns,subset_ids='ALL',type=c('SIMPLE','EM')) {
  assert_av_state(av_state)
  if (missing(columns)) {
    stop("please supply a columns argument")
  }
  impute_methodr <- match.arg(type)
  impute_method <- switch(impute_methodr,
                          SIMPLE = impute_simple,
                          EM = impute_em
  )
  if (subset_ids == 'ALL') {
    subset_ids <- 1:length(av_state$data)
  }
  scat(av_state$log_level,2,
       paste("impute_missing_values: imputing values for columns: ",
             paste(columns,collapse=', '),"...\n",sep=""))
  missing_before <- calc_missing_all(av_state)
  for (subset_id in subset_ids) {
    if (class(subset_id) == 'numeric' && !any(subset_id == 1:length(av_state$data))) {
      warning(paste(subset_id,"does not identify a data set, skipping..."))
      next
    }
    data_frame <- av_state$data[[subset_id]]
    if (is.null(data_frame)) {
      warning(paste(subset_id,"does not identify a data set, skipping..."))
      next
    }
    missing_before_columns <- calc_missing_in_columns(av_state,subset_id,columns)
    for (column in columns) {
      data_column <- data_frame[[column]]
      if (is.null(data_column)) {
        stop(paste("column does not exist:",column,", skipping..."))
        next
      }
      av_state$data[[subset_id]][[column]] <- 
        impute_method(av_state$data[[subset_id]][[column]])
    }
    missing_after_columns <- calc_missing_in_columns(av_state,subset_id,columns)
    if (missing_before_columns$na_cnt != missing_after_columns$na_cnt) {
      scat(av_state$log_level,2,paste("impute_missing_values: imputed ",
                   missing_before_columns$string,
                   " missing values for subset ",subset_id,"\n",sep=""))
    }
  }
  missing_after <- calc_missing_all(av_state)
  if (missing_before != missing_after) {
    scat(av_state$log_level,2,
         paste("impute_missing_values: imputed a total of ",
               missing_before-missing_after," values.\n",sep=""))
  }
  av_state
}

calc_missing_in_columns <- function(av_state,subset_id,columns) {
  na_cnt <- 0
  tot_cnt <- 0
  max_perc <- 0
  for (column in columns) {
    na_cnt <- na_cnt + sum(is.na(av_state$data[[subset_id]][[column]]))
    tot_cnt <- tot_cnt + length(is.na(av_state$data[[subset_id]][[column]]))
    perc <- sum(is.na(av_state$data[[subset_id]][[column]]))/
            length(is.na(av_state$data[[subset_id]][[column]]))
    if (perc > max_perc) {
      max_perc <- perc
    }
  }
  list(tot_cnt=tot_cnt,
       na_cnt=na_cnt,
       string=paste(na_cnt," (<=",round(100*max_perc,digits=2),"%)",sep=""))
}

calc_missing <- function(data_frame) {
  na_cnt <- sum(is.na(data_frame))
  tot_cnt <- length(is.na(data_frame))
  paste(round(100*na_cnt/tot_cnt,digits=2),"% (",na_cnt,")",sep="")
}

calc_missing_all <- function(av_state) {
  na_cnt <- 0
  for (subset_id in 1:length(av_state$data)) {
    if (subset_id > length(av_state$data)) { break }
    na_cnt <- na_cnt+sum(is.na(av_state$data[[subset_id]]))
  }
  na_cnt
}

impute_em <- function(column_data) {
  warning("impute_em: em imputation not implemented.")
  column_data
}

impute_simple <- function(column_data) {
  # check whether there are NA values:
  if (any(is.na(column_data))) {
    # what type is the column?
    if (class(column_data) == "factor") {
      impute_simple_factor(column_data)
    } else if (class(column_data) == "numeric") {
      impute_simple_numeric(column_data)
    } else {
      warning(paste("unknown column class",
                    class(column_data),
                    "for column"))
      column_data
    }
  } else {
    column_data
  }
}

impute_simple_factor <- function(column_data) {
  na_vals <- which(is.na(column_data))
  for (idx in na_vals) {
    surr <- surrounding_vals(column_data,idx)
    val <- mode_value(column_data[surr])
    column_data[idx] <- val
  }
  column_data
}

mode_value <- function(x) {
  names(sort(-table(x)))[1]
}

surrounding_vals <- function(column_data,idx) {
  indices <-  which(!is.na(column_data))
  sortres <- sort(c(idx,indices),index.return=TRUE)
  index <- which(sortres$ix == 1)
  lefttail <- indices[max(index-2,1):index-1]
  righttail <- NULL
  if (index <= length(indices)) {
    if (index == 1) {
      righttail <- indices[index:min(index+2,length(indices))]
    } else {
      righttail <- indices[index:min(index+1,length(indices))]
    }
  }
  c(lefttail,righttail)
}

impute_simple_numeric <- function(column_data) {
  na_vals <- which(is.na(column_data))
  for (idx in na_vals) {
    surr <- surrounding_vals(column_data,idx)
    val <- mean(column_data[surr])
    column_data[idx] <- val
  }
  column_data
}
