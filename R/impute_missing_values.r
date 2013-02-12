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
  scat(2,paste("impute_missing_values: imputing values for columns: ",paste(columns,collapse=', '),"...\n",sep=""))
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
    missing_before_columns <- calc_missing_in_columns(subset_id,columns)
    for (column in columns) {
      data_column <- data_frame[[column]]
      if (is.null(data_column)) { 
        stop(paste("column does not exist:",column,", skipping..."))
        next
      }
      impute_method(subset_id,column)
    }
    missing_after_columns <- calc_missing_in_columns(subset_id,columns)
    if (missing_before_columns$na_cnt != missing_after_columns$na_cnt) {
      scat(2,paste("impute_missing_values: imputed ",
                   missing_before_columns$string,
                   " missing values for subset ",subset_id,"\n",sep=""))
    }
  }
  missing_after <- calc_missing_all()
  if (missing_before != missing_after) {
    scat(2,paste("impute_missing_values: imputed a total of ",missing_before-missing_after," values.\n",sep=""))
  }
}

calc_missing_in_columns <- function(subset_id,columns) {
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

impute_simple_numeric <- function(subset_id,column) {
  na_vals <- which(is.na(av_state$data[[subset_id]][[column]]))
  for (idx in na_vals) {
    surr <- surrounding_vals(subset_id,column,idx)
    val <- mean(av_state$data[[subset_id]][[column]][surr])
    av_state$data[[subset_id]][[column]][idx] <<- val
  }
}
