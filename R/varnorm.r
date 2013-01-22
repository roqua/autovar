# varnorm <-> Jarque-Bera <-> normality.test()
# The Skewness, Kurtosis, and JB tests (of the residuals)
# cannot be significant

powerset <- function(lst) {
  res <- NULL
  for (i in 1:length(lst)) {
    cmbs <- combn(lst,i)
    for (j in 1:(dim(cmbs)[[2]])) {
      res <- c(res,list(cmbs[,j]))
    }
  }
  res
}

sample_kurtosis <- function(x) {
  n <- length(x)
  smu <- mean(x)
  sumfourths <- 0
  sumquads <- 0
  for (i in 1:n) {
    sumfourths <- sumfourths + (x[[i]] - smu)^4
    sumquads <- sumquads + (x[[i]] - smu)^2
  }
  num <- (1/n)*sumfourths
  dnom <- ((1/n)*sumquads)^2
  sk <- (num/dnom)
  chii <- ((sk-3)^2)*n/24
  data.frame(Kurtosis=sk,chi2=chii,df=1,P=chi_squared_prob(chii,1))
}

sample_skewness <- function(x) {
  n <- length(x)
  smu <- mean(x)
  sumfourths <- 0
  sumquads <- 0
  for (i in 1:n) {
    sumfourths <- sumfourths + (x[[i]] - smu)^3
    sumquads <- sumquads + (x[[i]] - smu)^2
  }
  num <- (1/n)*sumfourths
  dnom <- ((1/n)*sumquads)^(3/2)
  sk <- (num/dnom)
  chii <- (sk^2)*n/6
  data.frame(Skewness=sk,chi2=chii,df=1,P=chi_squared_prob(chii,1))
}

jb_test <- function(x) {
  n <- length(x)
  s <- sample_skewness(x)
  k <- sample_kurtosis(x)
  jb <- (n/6)*(s$Skewness^2 + (1/4)*(k$Kurtosis-3)^2)
  df <- s$df+k$df
  data.frame(chi2=jb,df=df,P=chi_squared_prob(jb,df))
}

jb <- function(varest) {
  resids <- resid(varest)
  names <- dimnames(resids)[[2]]
  
  # JB test
  jbtab <- NULL
  for (i in 1:(dim(resids)[2])) {
    x <- resids[,i]
    if (i == 1) {
      jbtab <- jb_test(x)
    } else {
      jbtab <- rbind(jbtab,jb_test(x))
    }
  }
  if (!is.null(jbtab)) {
    chi2 <- sum(jbtab$chi2)
    df <- sum(jbtab$df)
    jbtab <- rbind(jbtab,data.frame(chi2=chi2,
                                df=df,
                                P=chi_squared_prob(chi2,df)))
    names <- c(names,'ALL')
    dimnames(jbtab)[[1]] <- names
  }
  
  # Skewness test
  sktab <- NULL
  names <- dimnames(resids)[[2]]
  for (i in 1:(dim(resids)[2])) {
    x <- resids[,i]
    if (i == 1) {
      sktab <- sample_skewness(x)
    } else {
      sktab <- rbind(sktab,sample_skewness(x))
    }
  }
  if (!is.null(sktab)) {
    chi2 <- sum(sktab$chi2)
    df <- sum(sktab$df)
    sktab <- rbind(sktab,
                   data.frame(Skewness=NA,
                              chi2=chi2,
                                df=df,
                                P=chi_squared_prob(chi2,df)))
    names <- c(names,'ALL')
    dimnames(sktab)[[1]] <- names
  }
  
  # Kurtosis test
  kttab <- NULL
  names <- dimnames(resids)[[2]]
  for (i in 1:(dim(resids)[2])) {
    x <- resids[,i]
    if (i == 1) {
      kttab <- sample_kurtosis(x)
    } else {
      kttab <- rbind(kttab,sample_kurtosis(x))
    }
  }
  if (!is.null(kttab)) {
    chi2 <- sum(kttab$chi2)
    df <- sum(kttab$df)
    kttab <- rbind(kttab,
                   data.frame(Kurtosis=NA,
                              chi2=chi2,
                              df=df,
                              P=chi_squared_prob(chi2,df)))
    names <- c(names,'ALL')
    dimnames(kttab)[[1]] <- names
  }
  
  list(jb=jbtab,sk=sktab,kt=kttab)
}

varnorm <- function(varest) {
  r <- jb(varest)
  ret_fail_columns <- NULL
  
  cat("\nJarque-Bera test\n")
  print(r$jb)
  names <- dimnames(r$jb)[[1]]
  fails <- NULL
  for (i in 1:(dim(r$jb)[1])) {
    if (r$jb$P[[i]] <= av_state$significance) {
      column <- names[i]
      fails <- c(fails,column)
      if (column == 'ALL') {
        ret_fail_columns <- av_state$vars
      } else {
        real_column <- unprefix_ln(column)
        if (!(real_column %in% ret_fail_columns)) {
          ret_fail_columns <- c(ret_fail_columns,real_column)
        }
      }
    }
  }
  if (!is.null(fails)) {
    cat("  Failed for: ",paste(fails,collapse=', '),'\n',sep='')
  }
  
  cat("\nSkewness test\n")
  print(r$sk)
  names <- dimnames(r$sk)[[1]]
  fails <- NULL
  for (i in 1:(dim(r$sk)[1])) {
    if (r$sk$P[[i]] <= av_state$significance) {
      column <- names[i]
      fails <- c(fails,column)
      if (column == 'ALL') {
        ret_fail_columns <- av_state$vars
      } else {
        real_column <- unprefix_ln(column)
        if (!(real_column %in% ret_fail_columns)) {
          ret_fail_columns <- c(ret_fail_columns,real_column)
        }
      }
    }
  }
  if (!is.null(fails)) {
    cat("  Failed for: ",paste(fails,collapse=', '),'\n',sep='')
  }
  
  cat("\nKurtosis test\n")
  print(r$kt)
  names <- dimnames(r$kt)[[1]]
  fails <- NULL
  for (i in 1:(dim(r$kt)[1])) {
    if (r$kt$P[[i]] <= av_state$significance) {
      column <- names[i]
      fails <- c(fails,column)
      if (column == 'ALL') {
        ret_fail_columns <- av_state$vars
      } else {
        real_column <- unprefix_ln(column)
        if (!(real_column %in% ret_fail_columns)) {
          ret_fail_columns <- c(ret_fail_columns,real_column)
        }
      }
    }
  }
  if (!is.null(fails)) {
    cat("  Failed for: ",paste(fails,collapse=', '),'\n',sep='')
  }
  
  if (is.null(ret_fail_columns)) {
    cat("PASS: Unable to reject null hypothesis that residuals are normally distributed.\n")
  } else {
    cat("FAIL: Residuals are significantly not normally distributed.\n")
  }
  if (!is.null(ret_fail_columns)) {
    ret_fail_columns <- powerset(ret_fail_columns)
  }
  ret_fail_columns
}
