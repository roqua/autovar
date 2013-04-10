# unused_exogenous
unused_exogenous2 <- function(exogenous_variables,varest) {
  # return a vector of column names to be removed from exogenous_variables
  unused_vars <- exogenous_variables
  min_pvalue <- av_state_significance(varest)
  for (eqname in restriction_matrix_rownames(varest)) {
    summ <- summary(varest)
    coefs <- summ$varresult[[eqname]]$coefficients
    for (i in 1:(dim(coefs)[[1]])) {
      varname <- rownames(coefs)[[i]]
      if (any(varname == exogenous_variables)) {
        pvalue <- coefs[i,4]
        if (pvalue <= min_pvalue) {
          unused_vars <- unused_vars[varname != unused_vars]
        }
      }
    }
  }
  unused_vars
}

unused_exogenous <- function(exogenous_variables,varest) {
  # return a vector of column names to be removed from exogenous_variables
  unused_varnames <- NULL
  min_pvalue <- av_state_significance(varest)
  least_signif_exovar <- least_signif_exovar(exogenous_variables,varest)
  while (!is.null(least_signif_exovar) && least_signif_exovar > min_pvalue) {
    varname <- names(least_signif_exovar)
    unused_varnames <- c(unused_varnames,varname)
    resmat <- new_restriction_matrix(varest)
    for (eqname in restriction_matrix_rownames(varest)) {
      resmat <- update_restriction_matrix(varest,eqname,varname,0,resmat)
    }
    varest <- restrict(varest,
                       method="manual",
                       resmat=format_restriction_matrix(varest,resmat))
    least_signif_exovar <- least_signif_exovar(exogenous_variables,varest)
  }
  unused_varnames
}

least_signif_exovar <- function(exogenous_variables,varest) {
  used_vars <- list()
  summ <- summary(varest)
  for (eqname in restriction_matrix_rownames(varest)) {
    coefs <- summ$varresult[[eqname]]$coefficients
    for (i in 1:(dim(coefs)[[1]])) {
      varname <- rownames(coefs)[[i]]
      if (any(varname == exogenous_variables)) {
        pvalue <- coefs[i,4]
        used_vars[[varname]] <- c(used_vars[[varname]],pvalue)
      }
    }
  }
  # sort by highest minimum val
  if (length(used_vars) > 0) {
    least_signif_day <- sort(sapply(used_vars,min),decreasing=TRUE)[1]
    least_signif_day
  } else {
    NULL
  }
}
