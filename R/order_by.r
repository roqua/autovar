# order_by

# order_by defines the chronological order in which fields are seen
order_by <- function(id_field,impute_method=c('ONE_MISSING','ADD_MISSING','NONE')) {
  av_state$impute_method <<- match.arg(impute_method)
  av_state$order_by <<- id_field
  order_method <- switch(av_state$impute_method,
    ONE_MISSING = order_by_impute_one_missing,
    ADD_MISSING = order_by_impute_add_missing,
    NONE = order_by_impute_none
  )
  i <- 0
  for (data_frame in av_state$data) {
    i <- i+1
    av_state$data[[i]] <<- order_method(id_field,data_frame)
  }
}

order_by_impute_one_missing <- function(id_field,data_frame) {
  if (any(is.na(getElement(data_frame,id_field)))) {
    if (sum(is.na(getElement(data_frame,id_field))) != 1) {
      stop("More than one field is NA")
    }
    imputed_val <- missing_in_range(getElement(data_frame,id_field))
    if (is.null(imputed_val)) {
      cat("order_by_impute_one_missing imputed",imputed_val,"for one row of",frame_identifier(data_frame),"\n")
      data_frame[is.na(getElement(data_frame,id_field)),][[id_field]] <- imputed_val
    }
  }
  data_frame[with(data_frame, order(getElement(data_frame,id_field))), ]
}

frame_identifier <- function(data_frame) {
  if (is.null(av_state[['group_by']])) {
    ""
  } else {
    id_field <- av_state[['group_by']]
    paste(id_field,' = ',data_frame[[id_field]][1],sep='')
  }
}

missing_in_range <- function(sorting_column) {
  ordered_column <- sort(sorting_column)
  mmin <- min(ordered_column)
  mmax <- max(ordered_column)
  last_elem <- NA
  ldiff <- NA
  for (elem in ordered_column) {
    if (!is.na(last_elem)) {
      cdiff <- last_elem - elem
      if (!is.na(ldiff)) {
        
      }
      ldiff <- cdiff
    }
    last_elem <- elem
  }
  diffs <- ordered_column[2:length(ordered_column)]-ordered_column[1:length(ordered_column)-1]
  tab <- table(diffs)
  if (length(order(tab)) == 1) {
    mmax+1
  } else {
    infreq <- order(tab)[[1]]
    freq <- order(tab)[[2]]
    idx <- which(diffs == infreq)
    if (length(idx) == 0) {
      warning("could not determine a valid substitute for the NA value")
      NULL
    } else {
      ordered_column[idx]+freq
    }
  }
}

order_by_impute_none <- function(id_field,data_frame) {
  sorting_column <- getElement(data_frame,id_field)
  if (any(is.na(sorting_column))) {
    warning("Some fields are NA")
  }
  data_frame[with(data_frame, order(sorting_column)), ]
}

order_by_impute_add_missing <- function(id_field,data_frame) {
  sorting_column <- getElement(data_frame,id_field)
  if (any(is.na(sorting_column))) {
    halt(paste("Some fields are NA, they need to be assigned an",id_field,"before we can determine which rows are missing."))
  }
  ordered_column <- sort(sorting_column)
  mmin <- min(ordered_column)
  mmax <- max(ordered_column)
  gbv <- NULL
  if (!is.null(av_state$group_by)) {
    gbv <- data_frame[1,][[av_state$group_by]]
  }
  for(i in mmin:mmax) {
    if (!any(sorting_column == i)) {
      cat(paste("order_by: adding a row with",id_field,"=",i,"\n"))
      data_frame <- rbind(data_frame,rep(NA,times=dim(data_frame)[[2]]))
      data_frame[dim(data_frame)[[1]],][[id_field]] <- i
      if (!is.null(av_state$group_by)) {
        data_frame[dim(data_frame)[[1]],][[av_state$group_by]] <- gbv
      }
    }
  }
  data_frame[with(data_frame, order(getElement(data_frame,id_field))), ]
}

#print(order_by_impute_add_missing('tijdstip',data.frame(id=rep(1,times=5),tijdstip=c(1,3,5,6,7),home=6:10)))
