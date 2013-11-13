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

myplot.igraph <- function (x, incenter = FALSE, axes = FALSE, xlab = "", ylab = "", add = FALSE, 
          xlim = c(-1, 1), ylim = c(-1, 1), main = "", sub = "", mark.groups = list(), 
          mark.shape = 1/2, mark.col = rainbow(length(mark.groups), 
                                               alpha = 0.3), mark.border = rainbow(length(mark.groups), 
                                                                                   alpha = 1), mark.expand = 15, ...) 
{
  graph <- x
  if (!is.igraph(graph)) {
    stop("Not a graph object")
  }
  params <- igraph:::i.parse.plot.params(graph, list(...))
  vertex.size <- 1/200 * params("vertex", "size")
  label.family <- params("vertex", "label.family")
  label.font <- params("vertex", "label.font")
  label.cex <- params("vertex", "label.cex")
  label.degree <- params("vertex", "label.degree")
  label.color <- params("vertex", "label.color")
  label.dist <- params("vertex", "label.dist")
  labels <- params("vertex", "label")
  shape <- igraph:::igraph.check.shapes(params("vertex", "shape"))
  edge.color <- params("edge", "color")
  edge.width <- params("edge", "width")
  edge.lty <- params("edge", "lty")
  arrow.mode <- params("edge", "arrow.mode")
  edge.labels <- params("edge", "label")
  loop.angle <- params("edge", "loop.angle")
  edge.label.font <- params("edge", "label.font")
  edge.label.family <- params("edge", "label.family")
  edge.label.cex <- params("edge", "label.cex")
  edge.label.color <- params("edge", "label.color")
  arrow.size <- params("edge", "arrow.size")[1]
  arrow.width <- params("edge", "arrow.width")[1]
  curved <- params("edge", "curved")
  if (is.function(curved)) {
    curved <- curved(graph)
  }
  layout <- params("plot", "layout")
  margin <- params("plot", "margin")
  margin <- rep(margin, length = 4)
  rescale <- params("plot", "rescale")
  asp <- params("plot", "asp")
  frame <- params("plot", "frame")
  arrow.mode <- igraph:::i.get.arrow.mode(graph, arrow.mode)
  maxv <- max(vertex.size)
  if (rescale) {
    layout <- layout.norm(layout, -1, 1, -1, 1)
    xlim <- c(xlim[1] - margin[2] - maxv, xlim[2] + margin[4] + 
                maxv)
    ylim <- c(ylim[1] - margin[1] - maxv, ylim[2] + margin[3] + 
                maxv)
  }
  if (!add) {
    plot(0, 0, type = "n", xlab = xlab, ylab = ylab, xlim = xlim, 
         ylim = ylim, axes = axes, frame = frame, asp = asp, 
         main = main, sub = sub)
  }
  if (!is.list(mark.groups) && is.numeric(mark.groups)) {
    mark.groups <- list(mark.groups)
  }
  mark.shape <- rep(mark.shape, length = length(mark.groups))
  mark.border <- rep(mark.border, length = length(mark.groups))
  mark.col <- rep(mark.col, length = length(mark.groups))
  mark.expand <- rep(mark.expand, length = length(mark.groups))
  for (g in seq_along(mark.groups)) {
    v <- mark.groups[[g]]
    if (length(vertex.size) == 1) {
      vs <- vertex.size
    }
    else {
      vs <- rep(vertex.size, length = vcount(graph))[v]
    }
    igraph:::igraph.polygon(layout[v, , drop = FALSE], vertex.size = vs, 
                   expand.by = mark.expand[g]/200, shape = mark.shape[g], 
                   col = mark.col[g], border = mark.border[g])
  }
  el <- get.edgelist(graph, names = FALSE)
  loops.v <- el[, 1][el[, 1] == el[, 2]]
  loops.e <- which(el[, 1] == el[, 2])
  nonloops.e <- which(el[, 1] != el[, 2])
  loop.labels <- edge.labels[el[, 1] == el[, 2]]
  edge.labels <- edge.labels[el[, 1] != el[, 2]]
  el <- el[el[, 1] != el[, 2], , drop = FALSE]
  edge.coords <- matrix(0, nrow = nrow(el), ncol = 4)
  edge.coords[, 1] <- layout[, 1][el[, 1]]
  edge.coords[, 2] <- layout[, 2][el[, 1]]
  edge.coords[, 3] <- layout[, 1][el[, 2]]
  edge.coords[, 4] <- layout[, 2][el[, 2]]
  if (length(unique(shape)) == 1) {
    ec <- igraph:::.igraph.shapes[[shape[1]]]$clip(edge.coords, el, 
                                          params = params, end = "both")
  }
  else {
    shape <- rep(shape, length = vcount(graph))
    ec <- edge.coords
    ec[, 1:2] <- t(sapply(seq(length = nrow(el)), function(x) {
      igraph:::.igraph.shapes[[shape[el[x, 1]]]]$clip(edge.coords[x, 
                                                         , drop = FALSE], el[x, , drop = FALSE], params = params, 
                                             end = "from")
    }))
    ec[, 3:4] <- t(sapply(seq(length = nrow(el)), function(x) {
      igraph:::.igraph.shapes[[shape[el[x, 2]]]]$clip(edge.coords[x, 
                                                         , drop = FALSE], el[x, , drop = FALSE], params = params, 
                                             end = "to")
    }))
  }
  x0 <- ec[, 1]
  y0 <- ec[, 2]
  x1 <- ec[, 3]
  y1 <- ec[, 4]
  if (length(loops.e) > 0) {
    ec <- edge.color
    if (length(ec) > 1) {
      ec <- ec[loops.e]
    }
    point.on.cubic.bezier <- function(cp, t) {
      c <- 3 * (cp[2, ] - cp[1, ])
      b <- 3 * (cp[3, ] - cp[2, ]) - c
      a <- cp[4, ] - cp[1, ] - c - b
      t2 <- t * t
      t3 <- t * t * t
      a * t3 + b * t2 + c * t + cp[1, ]
    }
    compute.bezier <- function(cp, points) {
      dt <- seq(0, 1, by = 1/(points - 1))
      sapply(dt, function(t) point.on.cubic.bezier(cp, 
                                                   t))
    }
    plot.bezier <- function(cp, points, color, width, arr, 
                            lty, arrow.size, arr.w) {
      p <- compute.bezier(cp, points)
      polygon(p[1, ], p[2, ], border = color, lwd = width, 
              lty = lty)
      if (arr == 1 || arr == 3) {
        igraph:::igraph.Arrows(p[1, ncol(p) - 1], p[2, ncol(p) - 
                                             1], p[1, ncol(p)], p[2, ncol(p)], sh.col = color, 
                      h.col = color, size = arrow.size, sh.lwd = width, 
                      h.lwd = width, open = FALSE, code = 2, width = arr.w)
      }
      if (arr == 2 || arr == 3) {
        igraph:::igraph.Arrows(p[1, 2], p[2, 2], p[1, 1], p[2, 
                                                   1], sh.col = color, h.col = color, size = arrow.size, 
                      sh.lwd = width, h.lwd = width, open = FALSE, 
                      code = 2, width = arr.w)
      }
    }
    loop <- function(x0, y0, cx = x0, cy = y0, color, angle = 0, 
                     label = NA, width = 1, arr = 2, lty = 1, arrow.size = arrow.size, 
                     arr.w = arr.w) {
      rad <- angle
      center <- c(cx, cy)
      cp <- matrix(c(x0, y0, x0 + 0.4, y0 + 0.2, x0 + 
                       0.4, y0 - 0.2, x0, y0), ncol = 2, byrow = TRUE)
      phi <- atan2(cp[, 2] - center[2], cp[, 1] - center[1])
      r <- sqrt((cp[, 1] - center[1])^2 + (cp[, 2] - center[2])^2)
      phi <- phi + rad
      cp[, 1] <- cx + r * cos(phi)
      cp[, 2] <- cy + r * sin(phi)
      plot.bezier(cp, 50, color, width, arr = arr, lty = lty, 
                  arrow.size = arrow.size, arr.w = arr.w)
      if (is.language(label) || !is.na(label)) {
        lx <- x0 + 0.3
        ly <- y0
        phi <- atan2(ly - center[2], lx - center[1])
        r <- sqrt((lx - center[1])^2 + (ly - center[2])^2)
        phi <- phi + rad
        lx <- cx + r * cos(phi)
        ly <- cy + r * sin(phi)
        text(lx, ly, label, col = edge.label.color, 
             font = edge.label.font, family = edge.label.family, 
             cex = edge.label.cex)
      }
    }
    ec <- edge.color
    if (length(ec) > 1) {
      ec <- ec[loops.e]
    }
    vs <- vertex.size
    if (length(vertex.size) > 1) {
      vs <- vs[loops.v]
    }
    ew <- edge.width
    if (length(edge.width) > 1) {
      ew <- ew[loops.e]
    }
    la <- loop.angle
    if (length(loop.angle) > 1) {
      la <- la[loops.e]
    }
    lty <- edge.lty
    if (length(edge.lty) > 1) {
      lty <- lty[loops.e]
    }
    arr <- arrow.mode
    if (length(arrow.mode) > 1) {
      arr <- arrow.mode[loops.e]
    }
    asize <- arrow.size
    if (length(arrow.size) > 1) {
      asize <- arrow.size[loops.e]
    }
    xx0 <- layout[loops.v, 1] + cos(la) * vs
    yy0 <- layout[loops.v, 2] - sin(la) * vs
    mapply(loop, xx0, yy0, color = ec, angle = -la, label = loop.labels, 
           lty = lty, width = ew, arr = arr, arrow.size = asize, 
           arr.w = arrow.width)
  }
  if (length(x0) != 0) {
    if (length(edge.color) > 1) {
      edge.color <- edge.color[nonloops.e]
    }
    if (length(edge.width) > 1) {
      edge.width <- edge.width[nonloops.e]
    }
    if (length(edge.lty) > 1) {
      edge.lty <- edge.lty[nonloops.e]
    }
    if (length(arrow.mode) > 1) {
      arrow.mode <- arrow.mode[nonloops.e]
    }
    if (length(arrow.size) > 1) {
      arrow.size <- arrow.size[nonloops.e]
    }
    if (length(curved) > 1) {
      curved <- curved[nonloops.e]
    }
    if (length(unique(arrow.mode)) == 1) {
      igraph:::igraph.Arrows(x0, y0, x1, y1, h.col = edge.color, 
                    sh.col = edge.color, sh.lwd = edge.width, h.lwd = 1, 
                    open = FALSE, code = arrow.mode[1], sh.lty = edge.lty, 
                    h.lty = 1, size = arrow.size, width = arrow.width, 
                    curved = curved)
    }
    else {
      curved <- rep(curved, length = ecount(graph))[nonloops.e]
      for (code in 0:3) {
        valid <- arrow.mode == code
        if (!any(valid)) {
          next
        }
        ec <- edge.color
        if (length(ec) > 1) {
          ec <- ec[valid]
        }
        ew <- edge.width
        if (length(ew) > 1) {
          ew <- ew[valid]
        }
        el <- edge.lty
        if (length(el) > 1) {
          el <- el[valid]
        }
        igraph:::igraph.Arrows(x0[valid], y0[valid], x1[valid], 
                      y1[valid], code = code, sh.col = ec, h.col = ec, 
                      sh.lwd = ew, h.lwd = 1, h.lty = 1, sh.lty = el, 
                      open = FALSE, size = arrow.size, width = arrow.width, 
                      curved = curved[valid])
      }
    }
    phi <- atan2(y1 - y0, x1 - x0)
    r <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
    x <- x0 + 2/3 * r * cos(phi)
    y <- y0 + 2/3 * r * sin(phi)
    if (length(incenter) > 1) {
      x[incenter] <- x0[incenter] + 1/2 * r[incenter] * cos(phi[incenter])
      y[incenter] <- y0[incenter] + 1/2 * r[incenter] * sin(phi[incenter])
      x[!incenter] <- x0[!incenter] + 3/4 * r[!incenter] * cos(phi[!incenter])
      y[!incenter] <- y0[!incenter] + 3/4 * r[!incenter] * sin(phi[!incenter])
    }
    
    text(x, y, labels = edge.labels, col = edge.label.color, 
         family = edge.label.family, font = edge.label.font, 
         cex = edge.label.cex)
  }
  rm(x0, y0, x1, y1)
  if (length(unique(shape)) == 1) {
    igraph:::.igraph.shapes[[shape[1]]]$plot(layout, params = params)
  }
  else {
    sapply(seq(length = vcount(graph)), function(x) {
      igraph:::.igraph.shapes[[shape[x]]]$plot(layout[x, , drop = FALSE], 
                                      v = x, params = params)
    })
  }
  par(xpd = TRUE)
  x <- layout[, 1] + label.dist * cos(-label.degree) * (vertex.size + 
                                                          6 * 8 * log10(nchar(labels) + 1))/200
  y <- layout[, 2] + label.dist * sin(-label.degree) * (vertex.size + 
                                                          6 * 8 * log10(nchar(labels) + 1))/200
  if (length(label.family) == 1) {
    text(x, y, labels = labels, col = label.color, family = label.family, 
         font = label.font, cex = label.cex)
  }
  else {
    if1 <- function(vect, idx) if (length(vect) == 1) 
      vect
    else vect[idx]
    sapply(seq_len(vcount(graph)), function(v) {
      text(x[v], y[v], labels = if1(labels, v), col = if1(label.color, 
                                                          v), family = if1(label.family, v), font = if1(label.font, 
                                                                                                        v), cex = if1(label.cex, v))
    })
  }
  rm(x, y)
  invisible(NULL)
}

# overriding the function
.onLoad <- function(libname,pkgname) {
  override_function("printCoefmat","stats",myprintCoefmat)
  override_function("print.varsum","vars",myprint.varsum)
  override_function("plot.igraph","igraph",myplot.igraph)
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
