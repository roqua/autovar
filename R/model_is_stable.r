# stability test
model_is_stable <- function(varest,log_level=0) {
  vsg <- varstable_graph(varest)
  scat(log_level,2,"\nEigenvalue stability condition\n")
  sprint(log_level,1,vsg)
  if (all(vsg$Modulus < 1)) {
    scat(log_level,2,
         "PASS: All the eigen values lie in the unit circle. VAR satisfies stability condition.\n")
    TRUE
  } else {
    scat(log_level,2,
         "FAIL: Not all the eigen values lie in the unit circle. VAR DOES NOT satisfy stability condition.\n")
    FALSE
  }
}

varstable_graph <- function(varest) {
  eigvals <- NULL
  suppressWarnings(tryCatch(eigvals <- roots(varest,modulus=FALSE),error=function(e) { }))
  mod_eigvals <- NULL
  suppressWarnings(tryCatch(mod_eigvals <- roots(varest,modulus=TRUE),error=function(e) { }))
  if (is.null(mod_eigvals) || is.null(eigvals)) {
    # This can occur if the matrix could not be inverted.
    varcount <- ncol(varest$y)
    eigvals <- rep(1,varcount)
    mod_eigvals <- rep(1,varcount)
  }
  ret <- data.frame(Eigenvalue=eigvals,Modulus=mod_eigvals, stringsAsFactors = TRUE)
  ret
}
