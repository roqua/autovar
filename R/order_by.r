#' Order the rows in a data set
#' 
#' This function determines the order of the data rows in the data set. For vector autoregression, you may want to use this to make sure that the data set is sorted by the date/time column, i.e., the supplied \code{id_field} parameter is often a measurement index (e.g., \code{'tijdstip'}). The \code{id_field} column has to be numeric. This function will also add a squared column to the data frame and include the order_by colum and its squared values as exogenous_variables.
#' @param av_state an object of class \code{av_state}
#' @param id_field the name of a column in the data set
#' @param impute_method this argument has four possible values: \itemize{
#' \item \code{'BEST_FIT'} - This is not an impute method itself, but tells the function to determine the optimal impute method and use that. This is the default choice for \code{impute_method} when it is not specified.
#' \item \code{'ONE_MISSING'} - Only works when the \code{id_field} in each data subset is an integer range with exactly one value missing and exactly one \code{NA} value. The \code{NA} value is then substituted by the missing index.
#' \item \code{'ADD_MISSING'} - Does not work when one or more rows have an \code{NA} value for \code{id_field}. Only works for integer ranges of \code{id_field} with single increments. Works by adding rows for all missing values in the range between the minimum and maximum value of \code{id_field}. All values in the added rows are \code{NA} except for the \code{id_field} and the field used for grouping the data (if there was one).
#' \item \code{'NONE'} - No imputation is performed.
#' }
#' @param add_as_exogenous determines whether the \code{order_by} column and its squared values should be used as exogenous variables in VAR models.
#' @return This function returns the modified \code{av_state} object. After the substitutions, the data sets in \code{av_state$data} are sorted by their \code{id_field} value. This sorting step moves any rows with value \code{NA} for the \code{id_field} to the end.
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav")
#' av_state <- order_by(av_state,'tijdstip',impute_method='ONE_MISSING')
#' @export
order_by <- function(av_state,id_field,impute_method=c('BEST_FIT','ONE_MISSING','ADD_MISSING','NONE'),
                     add_as_exogenous=FALSE) {
  assert_av_state(av_state)
  if (!is.null(av_state$order_by)) {
    stop("order_by can only be called once")
  }
  if (class(av_state$data[[1]][[id_field]]) != 'numeric') {
    stop(paste("id_field",id_field,"has to be numeric, but is",class(av_state$data[[1]][[id_field]])))
  }
  av_state$order_by <- id_field
  impute_method <- match.arg(impute_method)
  i <- 0
  missings <- {}
  nonsequential <- {}
  for (data_frame in av_state$data) {
    i <- i+1
    used_impute_method <- impute_method
    if (impute_method == 'BEST_FIT') {
      used_impute_method <- determine_impute_method(id_field,data_frame)
    }
    order_method <- switch(used_impute_method,
      ONE_MISSING = order_by_impute_one_missing,
      ADD_MISSING = order_by_impute_add_missing,
      NONE = order_by_impute_none
    )
    missing_before <- calc_missing(av_state$data[[i]])
    av_state$data[[i]] <- order_method(av_state$group_by,id_field,data_frame,av_state$log_level)
    missing_after <- calc_missing(av_state$data[[i]])
    if (missing_before != missing_after) {
      scat(av_state$log_level,2,paste("order_by: missing values went from ",
                missing_before," to ",missing_after," for subset ",i,
                " (",used_impute_method,")\n", sep=""))
    }
    nonsequential[i] <- is_nonsequential(av_state$data[[i]][[id_field]])
    missings[i] <- length(which(is.na(av_state$data[[i]][[id_field]])))
  }
  if (sum(missings) > 0) {
    warning(paste("order_by: some values of the",id_field,"column are still NA:",missings_report(av_state$group_by,missings,names(av_state$data))))
  }
  if (any(nonsequential)) {
    warning(paste("order_by: some subsets of the",id_field,"column are still nonsequential: subset ids:",names(av_state$data)[which(nonsequential)]))
  }
  if (add_as_exogenous) {
    sq_name <- paste(av_state$order_by,'2',sep='')
    av_state <- add_derived_column(av_state,sq_name,
                                   av_state$order_by,operation='SQUARED')
    av_state <- add_exogenous_variables(av_state,c(av_state$order_by,sq_name))
  }
  av_state
}

is_nonsequential <- function(column_data) {
  curval <- column_data[1]
  flag = FALSE
  i <- 2
  while (i <= length(column_data)) {
    new_val <- column_data[i]
    if (is.null(new_val) || is.null(curval) || new_val != curval+1) {
      flag = TRUE
      break
    }
    curval <- new_val
    i <- i+1
  }
  flag
}

missings_report <- function(group_by_field,missings,names) {
  msg <- ''
  i <- 0
  for (missing in missings) {
    i <- i+1
    if (missing > 0) {
      name <- names[i]
      if (msg != '') {
        msg <- paste(msg,', ',sep='')
      }
      msg <- paste(msg,group_by_field,' ',name,': ',missing,' missing',sep='')
    }
  }
  msg
}

determine_impute_method <- function(id_field,data_frame) {
  if (can_do_one_missing(id_field,data_frame)) {
    'ONE_MISSING'
  } else if (can_do_add_missing(id_field,data_frame)) {
    'ADD_MISSING'
  } else {
    'NONE'
  }
}
can_do_one_missing <- function(id_field,data_frame) {
  any(is.na(getElement(data_frame,id_field))) &&
  sum(is.na(getElement(data_frame,id_field))) == 1 &&
  !is.null(missing_in_range(data_frame[[id_field]],no_warn = TRUE))
}
can_do_add_missing <- function(id_field,data_frame) {
  !any(is.na(data_frame[[id_field]]))
}

order_by_impute_one_missing <- function(group_by_field,id_field,data_frame,log_level) {
  if (any(is.na(getElement(data_frame,id_field)))) {
    if (sum(is.na(getElement(data_frame,id_field))) != 1) {
      stop("More than one field is NA")
    }
    imputed_val <- missing_in_range(getElement(data_frame,id_field))
    if (!is.null(imputed_val)) {
      scat(log_level,2,"order_by_impute_one_missing imputed",imputed_val,
          "for one row of",frame_identifier(group_by_field,data_frame),"\n")
      data_frame[is.na(getElement(data_frame,id_field)),][[id_field]] <- imputed_val
    }
  }
  data_frame[with(data_frame, order(getElement(data_frame,id_field))), ]
}

frame_identifier <- function(group_by_field,data_frame) {
  if (is.null(group_by_field)) {
    ""
  } else {
    id_field <- group_by_field
    paste(id_field,' = ',data_frame[[id_field]][1],sep='')
  }
}

missing_in_range <- function(sorting_column, no_warn = FALSE) {
  ordered_column <- sort(sorting_column)
  mmin <- min(ordered_column)
  mmax <- max(ordered_column)
  diffs <- ordered_column[2:length(ordered_column)]-ordered_column[1:length(ordered_column)-1]
  tab <- table(diffs)
  if (length(order(tab)) == 1) {
    mmax+1
  } else {
    infreq <- order(tab)[[1]]
    freq <- order(tab)[[2]]
    idx <- which(diffs == infreq)
    if (length(idx) == 0) {
      if (!no_warn) { warning("could not determine a valid substitute for the NA value") }
      NULL
    } else {
      ordered_column[idx]+freq
    }
  }
}

order_by_impute_none <- function(group_by_field,id_field,data_frame,log_level) {
  sorting_column <- getElement(data_frame,id_field)
  data_frame[with(data_frame, order(sorting_column)), ]
}

order_by_impute_add_missing <- function(group_by_field,id_field,data_frame,log_level) {
  sorting_column <- getElement(data_frame,id_field)
  if (any(is.na(sorting_column))) {
    stop(paste("Some fields are NA, they need to be assigned an",
               id_field,"before we can determine which rows are missing."))
  }
  ordered_column <- sort(sorting_column)
  mmin <- min(ordered_column)
  mmax <- max(ordered_column)
  gbv <- NULL
  if (!is.null(group_by_field)) {
    gbv <- data_frame[1,][[group_by_field]]
  }
  for(i in mmin:mmax) {
    if (!any(sorting_column == i)) {
      scat(log_level,2,paste("order_by: adding a row with",id_field,"=",i,"\n"))
      data_frame <- rbind(data_frame,rep(NA,times=dim(data_frame)[[2]]))
      data_frame[dim(data_frame)[[1]],][[id_field]] <- i
      if (!is.null(group_by_field)) {
        data_frame[dim(data_frame)[[1]],][[group_by_field]] <- gbv
      }
    }
  }
  data_frame[with(data_frame, order(getElement(data_frame,id_field))), ]
}
