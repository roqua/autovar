create_model <- function(old_model,...) {
  res <- merge_lists(old_model,list(...))
  class(res) <- 'var_model'
  res
}

merge_lists <- function(old, new) {
  #nnames <- names(new)
  #rnames <- sort(unique(c(names(old), nnames)))
  #res <- sapply(rnames,
  #              function(i) { ifelse(i %in% nnames,new[[i]],old[[i]]) },
  #              simplify = FALSE)
  # the above implementation does not work for
  # complex objects (e.g., data frames)
  res <- list()
  nnames <- sort(unique(c(names(old), names(new))))
  for (nname in nnames) {
    if (nname %in% names(new)) {
      res[[nname]] <- new[[nname]]
    } else {
      res[[nname]] <- old[[nname]]
    }
  }
  res
}

lists_are_equal <- function(lsta,lstb) {
  if (!all(sort(names(lsta)) == sort(names(lstb)))) {
    FALSE
  } else {
    nn <- names(lsta)
    i <- 0
    matching <- TRUE
    for (value in lsta) {
      i <- i+1
      name <- nn[[i]]
      if (((is.null(lstb[[name]]) && !is.null(value)) || (is.null(value) &&
          !is.null(lstb[[name]]))) || (!is.null(dim(lstb[[name]])) && 
          !is.null(dim(value)) && dim(lstb[[name]]) != dim(value)) ||
          any(lstb[[name]] != value)) {
        matching <- FALSE
        break
      }
    }
    matching
  }
}
