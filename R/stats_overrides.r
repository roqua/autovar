# overriding a stats display function

myprintCoefmat <- function (x, digits = max(3L, getOption("digits") - 2L), signif.stars = getOption("show.signif.stars"), 
          signif.legend = signif.stars, dig.tst = max(1L, min(5L, 
                                                              digits - 1L)), cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(), 
          P.values = NULL, has.Pvalue = nc >= 4 && substr(colnames(x)[nc], 
                                                          1, 3) == "Pr(", eps.Pvalue = .Machine$double.eps, na.print = "NA", 
          ...) 
{
  x <- order_coefficients(x)
  if (is.null(d <- dim(x)) || length(d) != 2L) 
    stop("'x' must be coefficient matrix/data frame")
  nc <- d[2L]
  if (is.null(P.values)) {
    scp <- getOption("show.coef.Pvalues")
    if (!is.logical(scp) || is.na(scp)) {
      warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
      scp <- TRUE
    }
    P.values <- has.Pvalue && scp
  }
  else if (P.values && !has.Pvalue) 
    stop("'P.values' is TRUE, but 'has.Pvalue' is not")
  if (has.Pvalue && !P.values) {
    d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
    nc <- nc - 1
    has.Pvalue <- FALSE
  }
  else xm <- data.matrix(x)
  k <- nc - has.Pvalue - (if (missing(tst.ind)) 
    1
                          else length(tst.ind))
  if (!missing(cs.ind) && length(cs.ind) > k) 
    stop("wrong k / cs.ind")
  Cf <- array("", dim = d, dimnames = dimnames(xm))
  ok <- !(ina <- is.na(xm))
  for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
  if (length(cs.ind)) {
    acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
    if (any(ia <- is.finite(acs))) {
      digmin <- 1 + if (length(acs <- acs[ia & acs != 
                                            0])) 
        floor(log10(range(acs[acs != 0], finite = TRUE)))
      else 0
      Cf[, cs.ind] <- format(round(coef.se, max(1L, digits - 
                                                  digmin)), digits = digits)
    }
  }
  if (length(tst.ind)) 
    Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst), 
                            digits = digits)
  if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc)))) 
    for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
  ok[, tst.ind] <- FALSE
  okP <- if (has.Pvalue) 
    ok[, -nc]
  else ok
  x1 <- Cf[okP]
  dec <- getOption("OutDec")
  if (dec != ".") 
    x1 <- chartr(dec, ".", x1)
  x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
  if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
    Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1L, 
                                                                    digits - 1L))
  }
  if (any(ina)) 
    Cf[ina] <- na.print
  if (P.values) {
    if (!is.logical(signif.stars) || is.na(signif.stars)) {
      warning("option \"show.signif.stars\" is invalid: assuming TRUE")
      signif.stars <- TRUE
    }
    if (any(okP <- ok[, nc])) {
      pv <- as.vector(xm[, nc])
      Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst, 
                                 eps = eps.Pvalue)
      signif.stars <- signif.stars && any(pv[okP] < 0.1)
      if (signif.stars) {
        Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                         symbols = c("***", "**", "*", ".", " "))
        attr(Signif,"legend") <- "'***' <=0.001, '**' <=0.01, '*' <=0.05, '.' <=0.10"
        Cf <- cbind(Cf, format(Signif))
      }
    }
    else signif.stars <- FALSE
  }
  else signif.stars <- FALSE
  print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print, 
                ...)
  if (signif.stars && signif.legend) 
    cat("---\nSignif. codes:  ", attr(Signif, "legend"), 
        "\n", sep = "")
  invisible(x)
}

order_coefficients <- function(mtx) {
  dnames <- dimnames(mtx)[[1]]
  vars<-!is.na(str_locate(dnames,"\\.l[0-9]+$")[,1])
  matching_indices <- which(vars)
  nonmatching_indices <- which(!vars)
  reorder_m<-order(dnames[matching_indices])
  matching_indices<-matching_indices[reorder_m]
  sorted_all <- c(matching_indices,nonmatching_indices)
  mtx[sorted_all,]
}

myprint.varsum <- function (x, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), ...) {
  dim <- length(x$names)
  text1 <- "\nVAR Estimation Results:\n"
  cat(text1)
  row <- paste(rep("=", nchar(text1)), collapse = "")
  cat(row, "\n")
  cat(paste("Endogenous variables:", paste(colnames(x$covres), 
                                           collapse = ", "), "\n", collapse = " "))
  cat(paste("Deterministic variables:", paste(x$type, collapse = ", "), 
            "\n", collapse = " "))
  cat(paste("Sample size:", x$obs, "\n"))
  cat(paste("Log Likelihood:", round(x$logLik, 3), "\n"))
  cat("Roots of the characteristic polynomial:\n")
  cat(formatC(x$roots, digits = digits))
  cat("\nCall:\n")
  print(x$call)
  cat("\n\n")
  for (i in 1:dim) {
    result <- x$varresult[[x$names[i]]]
    text1 <- paste("Estimation results for equation ", x$names[i], 
                   ":", sep = "")
    cat(text1, "\n")
    row <- paste(rep("=", nchar(text1)), collapse = "")
    cat(row, "\n")
    text2 <- paste(x$names[i], " = ", paste(rownames(result$coef), 
                                            collapse = " + "), sep = "")
    cat(text2, "\n\n")
    printCoefmat(result$coef, digits = digits, signif.stars = signif.stars, 
                 na.print = "NA", ...)
    cat("\n")
    cat("\nResidual standard error:", format(signif(result$sigma, 
                                                    digits)), "on", result$df[2L], "degrees of freedom\n")
    if (!is.null(result$fstatistic)) {
      cat("Multiple R-Squared:", formatC(result$r.squared, 
                                         digits = digits))
      cat(",\tAdjusted R-squared:", formatC(result$adj.r.squared, 
                                            digits = digits), "\nF-statistic:", formatC(result$fstatistic[1], 
                                                                                        digits = digits), "on", result$fstatistic[2], 
          "and", result$fstatistic[3], "DF,  p-value:", 
          format.pval(pf(result$fstatistic[1L], result$fstatistic[2L], 
                         result$fstatistic[3L], lower.tail = FALSE), 
                      digits = digits), "\n")
    }
    cat("\n\n")
  }
  cat("\nCovariance matrix of residuals:\n")
  print(x$covres, digits = digits, ...)
  cat("\nCorrelation matrix of residuals:\n")
  print(significance_matrix(x), digits = digits, ...)
  cat("\n\n")
  invisible(x)
}

significance_matrix <- function(varsum) {
  a <- varsum$corres
  nresids <- length(varsum$varresult[[1]]$residuals)
  if (nresids == 0 || is.null(a) || any(dim(a) == 0)) return(a)
  n <- dim(a)[[1]]
  m <- dim(a)[[2]]
  r <- matrix(nrow=2*n,ncol=m)
  for (i in 1:n)
    for (j in 1:m) {
      if (i >= j) {
        r[2*i-1,j] <- a[i,j]
        r[2*i,j] <- significance_from_pearson_coef(a[i,j],nresids)
      } else {
        r[2*i,j] <- NA
        r[2*i-1,j] <- NA
      }
    }
  dimnames(r)[[2]] <- dimnames(a)[[2]]
  dn <- NULL
  for (i in 1:(length(dimnames(a)[[1]])))
    dn <- c(dn,dimnames(a)[[1]][[i]],"p")
  dimnames(r)[[1]] <- dn
  r
}
significance_from_pearson_coef <- function(p,n) {
  2*pt(abs(p)*sqrt(n-2)/sqrt(1-(p*p)),n-2,lower.tail=FALSE)
}

mysummary.lm <- function (object, correlation = FALSE, symbolic.cor = FALSE, 
                        ...) 
{
  z <- object
  p <- z$rank
  rdf <- z$df.residual
  if (p == 0) {
    r <- z$residuals
    n <- length(r)
    w <- z$weights
    if (is.null(w)) {
      rss <- sum(r^2)
    }
    else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
    class(ans) <- "summary.lm"
    ans$aliased <- is.na(coef(object))
    ans$residuals <- r
    ans$df <- c(0L, n, length(ans$aliased))
    ans$coefficients <- matrix(NA, 0L, 4L)
    dimnames(ans$coefficients) <- list(NULL, c("Estimate", 
                                               "Std. Error", "t value", "Pr(>|t|)"))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    return(ans)
  }
  if (is.null(z$terms)) 
    stop("invalid 'lm' object:  no 'terms' component")
  if (!inherits(object, "lm")) 
    warning("calling summary.lm(<fake-lm-object>) ...")
  Qr <- stats:::qr.lm(object)
  n <- NROW(Qr$qr)
  if (is.na(z$df.residual) || n - p != z$df.residual) 
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
  p1 <- 1L:p
  r <- z$residuals
  f <- z$fitted.values
  w <- z$weights
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept")) 
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  }
  else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    }
    else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[Qr$pivot[p1]]
  tval <- est/se
  ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
  ans$residuals <- r
  ans$coefficients <- cbind(est, se, tval, 2 * pnorm(abs(tval), lower.tail = FALSE))
  dimnames(ans$coefficients) <- list(names(z$coefficients)[Qr$pivot[p1]], 
                                     c("Estimate", "Std. Error", "t value", "Pr(>|z|)"))
  ans$aliased <- is.na(coef(object))
  ans$sigma <- sqrt(resvar)
  ans$df <- c(p, rdf, NCOL(Qr$qr))
  if (p != attr(z$terms, "intercept")) {
    df.int <- if (attr(z$terms, "intercept")) 
      1L
    else 0L
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - 
                                                       df.int)/rdf)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, 
                        numdf = p - df.int, dendf = rdf)
  }
  else ans$r.squared <- ans$adj.r.squared <- 0
  ans$cov.unscaled <- R
  dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 
                                                             1)]
  if (correlation) {
    ans$correlation <- (R * resvar)/outer(se, se)
    dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
    ans$symbolic.cor <- symbolic.cor
  }
  if (!is.null(z$na.action)) 
    ans$na.action <- z$na.action
  class(ans) <- "summary.lm"
  ans
}

# overriding the function
.onLoad <- function(libname,pkgname) {
  override_function("printCoefmat","stats",myprintCoefmat)
  override_function("print.varsum","vars",myprint.varsum)
  # uncomment the line below to have coefficients that 
  # correspond better to those calculated in stata
  # override_function("summary.lm","stats",mysummary.lm)
}

override_function <- function(origfuncname,packagename,newfunc) {
  failed <- FALSE
  tryCatch(unlockBinding(origfuncname, as.environment(paste("package:",packagename,sep=''))),
           error=function(e) failed <<- TRUE)
  if (!failed) assign(origfuncname, newfunc, paste("package:",packagename,sep=''))
  unlockBinding(origfuncname, getNamespace(packagename))
  assign(origfuncname, newfunc, getNamespace(packagename))
}
