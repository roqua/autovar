# create_new_model

create_new_model <- function(old_model,...) {
  # we are not removing varest because it shouldn't exist
  # in this range
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
  # fix for complex objects (e.g., data frames)
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
