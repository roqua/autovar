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
    extra_order <- NULL
    if (max(skews) < skew_threshold()) {
      extra_order <- order_by_quantity(extra_columns,-mssds)
    } else {
      extra_order <- order_by_quantity(extra_columns,skews)
    }
    extra_columns <- extra_columns[extra_order]
    rnames <- c(rnames,extra_columns)
  }
  if (length(rnames) > 6)
    rnames <- rnames[1:6]
  print(rnames)
  data[,rnames]
}

mssd_threshold <- function() {
  50
}
skew_threshold <- function() {
  0.6
}

order_by_quantity <- function(rnames,data) {
  r <- NULL
  for (rname in rnames) {
    value <- data[[rname]]
    if (!is.na(value))
      r <- c(r,value)
  }
  order(r)
}
z_skewness_columns <- function(df) {
  r <- NULL
  for (col in df)
    r <- c(r,abs(e1071::skewness(col,na.rm=TRUE,type=2)))
  names(r) <- names(df)
  r
}
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

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
