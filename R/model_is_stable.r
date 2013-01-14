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
  nvars <- varest$K
  mat <- matrix(nrow=nvars,ncol=nvars)
  i <- 0
  for (lm in varest$varresult) {
    i <- i+1
    mat[,i] <- lm$coefficients[1:nvars]
  }
  eigvals <- eigen(mat,only.values=TRUE)$values
  ret <- data.frame(Eigenvalue=eigvals,Modulus=Mod(eigvals))
  ret
}
