# create_new_model

create_new_model <- function(old_model,...) {
  # we are not removing varest because it shouldn't exist
  # in this range
  merge_lists(old_model,list(...))
}

merge_lists <- function(old, new) {
  nnames <- names(new)
  rnames <- sort(unique(c(names(old), nnames)))
  sapply(rnames,
         function(i) { ifelse(i %in% nnames,new[[i]],old[[i]]) },
         simplify = FALSE)
}
