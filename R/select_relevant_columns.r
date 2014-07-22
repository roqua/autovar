#' Select and return the relevant columns
#'
#' This function returns a subset of the columns in the given data frame that are considered most relevant for time series analysis.
#' @param data a data frame of 17 columns (ontspanning, opgewektheid, hier_en_nu, concentratie, beweging, iets_betekenen, humor, buiten_zijn, eigenwaarde, levenslust, onrust, somberheid, lichamelijk_ongemak, tekortschieten, piekeren, eenzaamheid, uw_eigen_factor) and 90 rows
#' @param failsafe does not include any pairs by default and simply returns the up to 6 column names with lowest z_skewness that have an MSSD above the threshold. \code{failsafe} defaults to \code{FALSE}.
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity.
#' @return This function returns the modified data frame consisting of at most 6 columns.
#' @export
select_relevant_columns <- function(data, failsafe = FALSE, log_level = 0) {
  mssds <- psych::mssd(data)
  rnames <- NULL
  if (failsafe) { # perform option b
    all_columns <- c('ontspanning', 'opgewektheid', 'hier_en_nu', 'concentratie',
                     'beweging', 'iets_betekenen', 'humor', 'buiten_zijn', 'eigenwaarde',
                     'levenslust', 'onrust', 'somberheid', 'lichamelijk_ongemak',
                     'tekortschieten', 'piekeren', 'eenzaamheid', 'uw_eigen_factor')
    remaining_columns <- select_mssd_columns(all_columns,mssds)
    if (length(remaining_columns) > 0) {
      df <- data[,remaining_columns]
      skews <- z_skewness_columns(df)
      remaining_order <- order_by_quantity_unbalanced(remaining_columns,skews)
      rnames <- c(rnames,remaining_columns[remaining_order])
    } else {
      return(NULL)
    }
    if (length(rnames) > 6)
      rnames <- rnames[1:6]
    return(data[,rnames])
  }
  skews <- z_skewness_columns(data)
  if (mssds[['uw_eigen_factor']] > mssd_threshold() &&
      skews[['uw_eigen_factor']] < skew_max_threshold())
    rnames <- c(rnames,'uw_eigen_factor')
  # adding three pairs
  pair1 <- c('opgewektheid','somberheid','somberheid')
  pair2 <- c('onrust','ontspanning','onrust')
  top_pair <- NULL
  top_pair_max_skewness <- 100000
  for (i in 1:length(pair1)) {
    if (!pair_is_valid(pair1[i],pair2[i],skews,mssds)) next
    pms <- pair_max_skewness(pair1[i],pair2[i],skews)
    if (pms < top_pair_max_skewness) {
      top_pair <- c(pair1[i],pair2[i])
      top_pair_max_skewness <- pms
    }
  }
  if (!is.null(top_pair)) {
    rnames <- c(rnames,top_pair)
  } else {
    # add one with lowest z_skew
    singles <- c('opgewektheid','somberheid','onrust','ontspanning')
    top_var <- NULL
    top_var_skew <- 100000
    for (varname in singles)
      if (skews[[varname]] < skew_max_threshold() &&
          mssds[[varname]] > mssd_threshold() &&
          skews[[varname]] < top_var_skew) {
        top_var_skew <- skews[[varname]]
        top_var <- varname
      }
    if (!is.null(top_var))
      rnames <- c(rnames,top_var)
  }
  remaining_columns <- c('hier_en_nu', 'concentratie', 'beweging',
                         'iets_betekenen', 'humor', 'buiten_zijn',
                         'eigenwaarde', 'levenslust', 'lichamelijk_ongemak',
                         'tekortschieten', 'piekeren', 'eenzaamheid')
  extra_columns <- select_mssd_columns(remaining_columns,mssds)
  if (length(extra_columns) > 0) {
    df <- data[,extra_columns]
    skews <- z_skewness_columns(df)
    bal <- sentiment_balance(rnames)
    extra_order <- NULL
    if (max(skews) < skew_min_threshold()) {
      extra_order <- order_by_quantity(extra_columns,-mssds,bal)
    } else {
      extra_order <- order_by_quantity(extra_columns,skews,bal)
    }
    extra_columns <- extra_columns[extra_order]
    rnames <- c(rnames,extra_columns)
  }
  if (length(rnames) > 6)
    rnames <- rnames[1:6]
  if (length(rnames) == 0)
    return(NULL)
  data[,rnames]
}

mssd_threshold <- function() {
  50
}
skew_min_threshold <- function() {
  0.6
}
skew_max_threshold <- function() {
  4
}

pair_is_valid <- function(var1,var2,skews,mssds) {
  skews[[var1]] < skew_max_threshold() && skews[[var2]] < skew_max_threshold() &&
    mssds[[var1]] > mssd_threshold() && mssds[[var2]] > mssd_threshold()
}
pair_max_skewness <- function(var1,var2,skews) {
  max(skews[[var1]],skews[[var2]])
}
sentiment_balance <- function(rnames) {
  r <- 0
  for (rname in rnames) {
    if (rname == 'uw_eigen_factor') next
    if (is_positive_property(rname)) r <- r+1
    else r <- r-1
  }
  r
}
order_by_quantity <- function(rnames,data,balance) {
  r <- NULL
  for (rname in rnames)
    r <- c(r,data[[rname]])
  norder <- order(r)
  posi <- 1
  negi <- 1
  rorder <- NULL
  while (posi <= length(norder) || negi <= length(norder)) {
    while (posi <= length(norder) && !is_positive_property(rnames[norder[posi]])) posi <- posi+1
    while (negi <= length(norder) &&  is_positive_property(rnames[norder[negi]])) negi <- negi+1
    if (negi > length(r) && posi <= length(r)) {
      rorder <- c(rorder,norder[posi])
    } else if (posi > length(r) && negi <= length(r)) {
      rorder <- c(rorder,norder[negi])
    } else if (posi > length(r) && negi > length(r)) {
      return(rorder)
    } else if (balance > 0) {
      balance <- balance-1
      rorder <- c(rorder,norder[negi])
      posi <- posi-1
    } else if (balance < 0) {
      balance <- balance+1
      rorder <- c(rorder,norder[posi])
      negi <- negi-1
    } else if (data[rnames[norder[posi]]] < data[rnames[norder[negi]]]) {
      rorder <- c(rorder,norder[posi],norder[negi])
    } else {
      rorder <- c(rorder,norder[negi],norder[posi])
    }
    posi <- posi+1
    negi <- negi+1
  }
  rorder
}
order_by_quantity_unbalanced <- function(rnames,data) {
  r <- NULL
  for (rname in rnames)
    r <- c(r,data[[rname]])
  order(r)
}
is_positive_property <- function(rname) {
  property_balance(rname) == 1
}
property_balance <- function(rname) {
  if (rname %in% c('opgewektheid','ontspanning','hier_en_nu',
                 'concentratie', 'beweging','iets_betekenen',
                 'humor', 'buiten_zijn','eigenwaarde', 'levenslust'))
    return(1)
  if (rname %in% c('onrust','somberheid','lichamelijk_ongemak',
                 'tekortschieten','piekeren','eenzaamheid'))
    return(-1)
  0
}
z_skewness_columns <- function(df) {
  r <- NULL
  for (col in df)
    r <- c(r,abs(e1071::skewness(col,na.rm=TRUE,type=2)/
                   se_skewness(length(na.omit(col)))))
  names(r) <- names(df)
  r
}
se_skewness <- function(n) {
  if (n <= 2) return(1)
  ses <- sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3)))
  if (ses == 0) return(1)
  ses
}

select_mssd_columns <- function(rnames, mssds) {
  r <- NULL
  for (rname in rnames)
    if (mssds[[rname]] > mssd_threshold())
      r <- c(r,rname)
  r
}
