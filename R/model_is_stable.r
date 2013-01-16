# STABILITY TESTS

model_is_stable <- function(varest) {
  vsg <- varstable_graph(varest)
  cat("\nEigenvalue stability condition\n")
  print(vsg)
  if (all(vsg$Modulus < 1)) {
    cat("PASS: All the eigen values lie in the unit circle. VAR satisfies stability condition.\n")
    TRUE
  } else {
    cat("FAIL: Not all the eigen values lie in the unit circle. VAR DOES NOT satisfy stability condition.\n")
    FALSE
  }
}

varstable_graph <- function(varest) {
  eigvals <- roots(varest,modulus=FALSE)
  mod_eigvals <- roots(varest,modulus=TRUE)
  ret <- data.frame(Eigenvalue=eigvals,Modulus=mod_eigvals)
  ret
}
