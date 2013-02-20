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
  eigvals <- roots(varest,modulus=FALSE)
  mod_eigvals <- roots(varest,modulus=TRUE)
  ret <- data.frame(Eigenvalue=eigvals,Modulus=mod_eigvals)
  ret
}
