# impute missing values

impute_missing_values <- function(columns,subset_ids='ALL',type=c('SIMPLE','EM')) {
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
  missing_before <- calc_missing_all()
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
    for (column in columns) {
      data_column <- data_frame[[column]]
      if (is.null(data_column)) { 
        stop(paste("column does not exist:",column,", skipping..."))
        next
      }
      impute_method(subset_id,column)
    }
  }
  missing_after <- calc_missing_all()
  if (missing_before != missing_after) {
    scat(2,paste("impute_missing_values: imputed data for ",missing_before-missing_after," NA values.\n",sep=""))
  }
}

calc_missing_all <- function() {
  na_cnt <- 0
  for (subset_id in 1:length(av_state$data)) {
    na_cnt <- na_cnt+sum(is.na(av_state$data[[subset_id]]))
  }
  na_cnt
}

impute_em <- function(subset_id,column) {
  warning("impute_em: em imputation not implemented.")
}

impute_simple <- function(subset_id,column) {
  #cat("impute_simple: subset_id",subset_id,"column:",column,"\n")
  # check whether there are NA values:
  if (any(is.na(av_state$data[[subset_id]][[column]]))) {
    # what type is the column?
    if (class(av_state$data[[subset_id]][[column]]) == "factor") {
      impute_simple_factor(subset_id,column)
    } else if (class(av_state$data[[subset_id]][[column]]) == "numeric") {
      impute_simple_numeric(subset_id,column)
    } else {
      warning(paste("unknown column class",
                    class(av_state$data[[subset_id]][[column]]),
                    "for column",column))
    }
  }
}

impute_simple_factor <- function(subset_id,column) {
  na_vals <- which(is.na(av_state$data[[subset_id]][[column]]))
  for (idx in na_vals) {
    surr <- surrounding_vals(subset_id,column,idx)
    val <- mode_value(av_state$data[[subset_id]][[column]][surr])
    av_state$data[[subset_id]][[column]][idx] <<- val
  }
}

mode_value <- function(x) {
  names(sort(-table(x)))[1]
}

surrounding_vals <- function(subset_id,column,idx) {
  indices <-  which(!is.na(av_state$data[[subset_id]][[column]]))
  sortres <- sort(c(idx,indices),index.return=TRUE)
  index <- which(sortres$ix == 1)
  lefttail <- indices[max(index-2,1):index-1]
  righttail <- indices[index:min(index+1,length(indices))]
  c(lefttail,righttail)
}

impute_simple_numeric <- function(subset_id,column) {
  na_vals <- which(is.na(av_state$data[[subset_id]][[column]]))
  for (idx in na_vals) {
    surr <- surrounding_vals(subset_id,column,idx)
    val <- mean(av_state$data[[subset_id]][[column]][surr])
    av_state$data[[subset_id]][[column]][idx] <<- val
  }
}
