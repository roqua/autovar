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
