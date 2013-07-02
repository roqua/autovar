# Skewness and Kurtosis test

varnorm2 <- function(varest,log_level=0) {
  r <- sktest(varest)
  ret_fail_columns <- NULL
  scat(log_level,2,"\nSkewness and Kurtosis tests (sktest)\n")
  sprint(log_level,1,r)
  names <- dimnames(r)[[1]]
  fails <- NULL
  for (i in 1:(dim(r)[1])) {
    if(i>(dim(r)[1])) { break }
    if (r$P[[i]] <= av_state_significance(varest)) {
      column <- names[i]
      fails <- c(fails,column)
      real_column <- unprefix_ln(column)
      if (!(real_column %in% ret_fail_columns)) {
        ret_fail_columns <- c(ret_fail_columns,real_column)
      }
    }
  }
  if (!is.null(fails)) {
    scat(log_level,1,"  Failed for: ",paste(fails,collapse=', '),'\n',sep='')
  }
  if (is.null(ret_fail_columns)) {
    scat(log_level,2,"PASS: Unable to reject null hypothesis that residuals are normally distributed.\n")
  } else {
    scat(log_level,2,"FAIL: Residuals are significantly not normally distributed.\n")
  }
  if (!is.null(ret_fail_columns)) {
    ret_fail_columns <- powerset(ret_fail_columns)
  }
  ret_fail_columns
}

sktest <- function(varest) {
  resids <- resid(varest)
  names <- dimnames(resids)[[2]]
  sks <- NULL
  for (i in 1:(dim(resids)[2])) {
    if(i>(dim(resids)[2])) { break }
    x <- resids[,i]
    g1 <- coefficient_of_skewness(x)
    b2 <- coefficient_of_kurtosis(x)
    n <- length(x)
    Z1 <- z_skewness(g1,n)
    Z2 <- z_kurtosis(b2,n)
    pp <- sktest_joint_p(Z1,Z2,n)
    skr <- data.frame(Obs=n,
                      Pr_Skewness=2-2*pnorm(abs(Z1)),
                      Pr_Kurtosis=2-2*pnorm(abs(Z2)),
                      adj_chi2=-2*log(pp),
                      P=pp)
    if (i == 1) {
      sks <- skr
    } else {
      sks <- rbind(sks,skr)
    }
  }
  dimnames(sks)[[1]] <- names
  sks
}

z_skewness <- function(g1,n) {
  Y <- g1 * sqrt((n+1)*(n+3)/(6*(n-2)))
  B2g1 <- (3*(n^2 + 27*n -70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
  W2 <- -1 + sqrt(2*(B2g1 - 1))
  a <- sqrt(2/(W2 -1))
  Z1 <- (1/(sqrt(log(sqrt(W2)))))*log((Y/a)+sqrt((Y/a)^2 +1))
  Z1
}

z_kurtosis <- function(b2,n) {
   Eb2 <- (3*(n-1)/(n+1))
   varb2 <- (24*n*(n-2)*(n-3))/((n+1)^2 * (n+3)*(n+5))
   X <- (b2 - Eb2) / sqrt(varb2)
   sqrtB1b2 <- (6*(n^2 - 5*n + 2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))
   A <- 6+ (8/sqrtB1b2)*((2/sqrtB1b2)+sqrt(1+(4/(sqrtB1b2^2))))
   Z2 <- (1/sqrt(2/(9*A)))*((1-(2/(9*A))) - ((1-(2/A))/(1+X*sqrt(2/(A-4))))^(1/3))
   Z2
}

sktest_joint_p <- function(Z1,Z2,n) {
  K2 <- Z1*Z1 + Z2*Z2
  ZC2 <- -qnorm(exp(-0.5*K2))
  logn <- log(n)
  cut <- 0.55*(n^0.2)-0.21
  a1 <- (-5+3.46*logn)*exp(-1.37*logn)
  b1 <- 1+(0.854-0.148*logn)*exp(-0.55*logn)
  b2mb1 <- 2.13/(1-2.37*logn)
  a2 <- a1-b2mb1*cut
  b2 <- b2mb1+b1
  Z <- NULL
  if (ZC2 < -1) {
    Z <- ZC2
  } else if (ZC2 < cut) {
    Z <- a1+b1*ZC2
  } else {
    Z <- a2+b2*ZC2
  }
  P <- 1 - pnorm(Z)
  P
}

coefficient_of_skewness <- function(x) {
  m3 <- rth_moment_about_the_mean(x,3)
  m2 <- rth_moment_about_the_mean(x,2)
  ccoef <- m3*m2^(-3/2)
  ccoef
}

coefficient_of_kurtosis <- function(x) {
  m4 <- rth_moment_about_the_mean(x,4)
  m2 <- rth_moment_about_the_mean(x,2)
  ccoef <- m4*m2^(-2)
  ccoef
}

rth_moment_about_the_mean <- function(x,r) {
  mmean <- mean(x)
  n <- length(x)
  tsum <- 0
  for (i in 1:n) {
    if(i>n) { break }
    tsum <- tsum + (x[[i]] - mmean)^(r)
  }
  tsum <- (1/n)*tsum
  tsum
}

