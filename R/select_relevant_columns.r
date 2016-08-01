#' Select and return the relevant columns
#'
#' This function returns a subset of the columns in the given data frame that are considered most relevant for time series analysis.
#' @param data a data frame of 17 columns (ontspanning, opgewektheid, hier_en_nu, concentratie, beweging, iets_betekenen, humor, buiten_zijn, eigenwaarde, levenslust, onrust, somberheid, lichamelijk_ongemak, tekortschieten, piekeren, eenzaamheid, uw_eigen_factor) and 90 rows
#' @param net_cfg a net_cfg object providing metadata about the networks
#' @param failsafe does not include any pairs by default and simply returns the up to 6 column names with lowest z_skewness that have an MSSD above the threshold. \code{failsafe} defaults to \code{FALSE}.
#' @param number_of_columns the maximum number of columns to return
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity.
#' @param force_include a single column name that should always be included in the column selection (regardless of mssd, z_skewness, failsafe).
#' @return This function returns the modified data frame consisting of at most 6 columns.
#' @examples
#' GN_COLUMNS <- c('ontspanning', 'opgewektheid', 'hier_en_nu', 'concentratie',
#'                 'beweging', 'iets_betekenen', 'humor', 'buiten_zijn',
#'                 'eigenwaarde', 'levenslust', 'onrust', 'somberheid',
#'                 'lichamelijk_ongemak', 'tekortschieten', 'piekeren', 'eenzaamheid',
#'                 'uw_eigen_factor')
#' data<-load_file("../data/input/DataDndN_nonimputed_voorAndo.sav")
#' data<-data$raw_data[,GN_COLUMNS]
#' net_cfg <- new_net_cfg()
#' net_cfg$vars <- unique(names(data))
#' net_cfg$always_include <- 'uw_eigen_factor'
#' net_cfg$pairs <- c('opgewektheid','onrust',
#'                    'somberheid','ontspanning',
#'                    'somberheid','onrust')
#' net_cfg$positive_variables <- c('opgewektheid','ontspanning','hier_en_nu',
#'                                 'concentratie', 'beweging','iets_betekenen',
#'                                 'humor', 'buiten_zijn','eigenwaarde', 'levenslust')
#' net_cfg$negative_variables <- c('onrust','somberheid','lichamelijk_ongemak',
#'                                 'tekortschieten','piekeren','eenzaamheid')
#' names(select_relevant_columns(data,net_cfg,FALSE,6))
#' @export
select_relevant_columns <- function(data, net_cfg, failsafe = FALSE, number_of_columns = 6, log_level = 0, force_include = NULL) {
  mssds <- psych::mssd(data)
  rnames <- NULL
  all_columns <- colnames(data)
  if (failsafe) { # perform option b
    if (!is.null(force_include))
      rnames <- c(rnames, force_include)
    remaining_columns <- all_columns
    remaining_columns <- remove_from_vector(remaining_columns,force_include)
    remaining_columns <- select_mssd_columns(remaining_columns,mssds)
    if (length(remaining_columns) > 0) {
      df <- data[remaining_columns]
      skews <- z_skewness_columns(df)
      remaining_order <- order_by_quantity_unbalanced(remaining_columns,skews)
      rnames <- c(rnames,remaining_columns[remaining_order])
    } else {
      return(NULL)
    }
    if (length(rnames) > number_of_columns)
      rnames <- rnames[1:number_of_columns]
    return(data[rnames])
  }
  # force include vars
  if (!is.null(force_include))
    rnames <- c(rnames, force_include)
  # always include vars
  skews <- z_skewness_columns(data)
  for (varname in net_cfg$always_include)
    if (mssds[[varname]] > mssd_threshold() &&
        skews[[varname]] < skew_max_threshold())
      rnames <- c(rnames,varname)
  # adding pairs
  if (!is.null(net_cfg$pairs) && length(net_cfg$pairs) > 1) {
    pair1 <- NULL
    pair2 <- NULL
    for (i in 1:length(net_cfg$pairs)) {
      if (i%%2 == 1) pair1 <- c(pair1,net_cfg$pairs[i])
      else pair2 <- c(pair2,net_cfg$pairs[i])
    }
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
      singles <- unique(net_cfg$pairs)
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
  }
  # calculate these are all vars minus pairs and minus always_include
  remaining_columns <- all_columns
  remaining_columns <- remove_from_vector(remaining_columns,force_include)
  remaining_columns <- remove_from_vector(remaining_columns,net_cfg$always_include)
  remaining_columns <- remove_from_vector(remaining_columns,net_cfg$pairs)
  extra_columns <- select_mssd_columns(remaining_columns,mssds)
  if (length(extra_columns) > 0) {
    df <- data[extra_columns]
    skews <- z_skewness_columns(df)
    bal <- sentiment_balance(rnames,net_cfg)
    extra_order <- NULL
    if (max(skews) < skew_min_threshold()) {
      extra_order <- order_by_quantity(extra_columns,-mssds,bal,net_cfg)
    } else {
      extra_order <- order_by_quantity(extra_columns,skews,bal,net_cfg)
    }
    extra_columns <- extra_columns[extra_order]
    rnames <- c(rnames,extra_columns)
  }
  if (length(rnames) > number_of_columns)
    rnames <- rnames[1:number_of_columns]
  if (length(rnames) == 0)
    return(NULL)
  data[rnames]
}

remove_from_vector <- function(a,b) {
  # remove b from a
  r = a[!(a %in% b)]
  if (length(r) == 0) return(NULL)
  r
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
sentiment_balance <- function(rnames,net_cfg) {
  r <- 0
  for (rname in rnames) {
    if (is_positive_property(rname,net_cfg)) r <- r+1
    else if (is_negative_property(rname,net_cfg)) r <- r-1
  }
  r
}
order_by_quantity <- function(rnames,data,balance,net_cfg) {
  r <- NULL
  for (rname in rnames)
    r <- c(r,data[[rname]])
  norder <- order(r)
  posi <- 1
  negi <- 1
  rorder <- NULL
  while (posi <= length(norder) || negi <= length(norder)) {
    # we treat variables that are not positive nor negative as negative here,
    # since we have more positive variables on average
    # (we don't have this problem if we include all neutral variables as 'always_include' variables)
    while (posi <= length(norder) && !is_positive_property(rnames[norder[posi]],net_cfg)) posi <- posi+1
    while (negi <= length(norder) &&  is_positive_property(rnames[norder[negi]],net_cfg)) negi <- negi+1
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
is_positive_property <- function(rname,net_cfg) {
  property_balance(rname,net_cfg) == 1
}
is_negative_property <- function(rname,net_cfg) {
  property_balance(rname,net_cfg) == -1
}
property_balance <- function(rname,net_cfg) {
  if (rname %in% net_cfg$positive_variables)
    return(1)
  if (rname %in% net_cfg$negative_variables)
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
