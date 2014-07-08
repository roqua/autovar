#' Select and return the relevant columns
#' 
#' This function returns a subset of the columns in the given data frame that are considered most relevant for time series analysis.
#' @param data a data frame of 17 columns (ontspanning, opgewektheid, hier_en_nu, concentratie, beweging, iets_betekenen, humor, buiten_zijn, eigenwaarde, levenslust, onrust, somberheid, lichamelijk_ongemak, tekortschieten, piekeren, eenzaamheid, uw_eigen_factor) and 90 rows
#' @return This function returns the modified data frame consisting of at most 6 columns.
#' @export
select_relevant_columns <- function(data) {
  rnames <- NULL
  mssds <- psych::mssd(data)
  
  if (mssds[['uw_eigen_factor']] > mssd_threshold())
    rnames <- c(rnames,'uw_eigen_factor')
  rnames <- add_max_of(rnames,mssds,'opgewektheid','somberheid')
  rnames <- add_max_of(rnames,mssds,'onrust','ontspanning')

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
    if (max(skews) < skew_threshold()) {
      extra_order <- order_by_quantity(extra_columns,-mssds,bal)
    } else {
      extra_order <- order_by_quantity(extra_columns,skews,bal)
    }
    extra_columns <- extra_columns[extra_order]
    rnames <- c(rnames,extra_columns)
  }
  if (length(rnames) > 6)
    rnames <- rnames[1:6]
  data[,rnames]
}

mssd_threshold <- function() {
  50
}
skew_threshold <- function() {
  0.6
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
is_positive_property <- function(rname) {
  positive_properties <- c('opgewektheid','ontspanning','hier_en_nu',
                           'concentratie', 'beweging','iets_betekenen',
                           'humor', 'buiten_zijn','eigenwaarde', 'levenslust')
  rname %in% positive_properties
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
add_max_of <- function(rnames, mssds, var1, var2) {
  max_name <- var1
  if (mssds[[var2]] > mssds[[max_name]])
    max_name <- var2
  if (mssds[[max_name]] > mssd_threshold())
    rnames <- c(rnames,max_name)
  rnames
}
select_mssd_columns <- function(rnames, mssds) {
  r <- NULL
  for (rname in rnames)
    if (mssds[[rname]] > mssd_threshold())
      r <- c(r,rname)
  r
}
