#' Impute missing values in a data.frame using EM imputation
#'
#' This function uses the Amelia package to impute a given data frame and return the imputed values.
#' @param df a data frame
#' @param net_cfg a net_cfg object providing metadata about the networks
#' @return This function returns the modified data frame.
#' @examples
#' data <- generate_numerical_test_data(40)
#' data
#' impute_dataframe(data,list(measurements_per_day=1))
#' @export
impute_dataframe <- function(df,net_cfg) {
  r <- impute_dataframe_aux(df,net_cfg)
  for (i in 1:29)
    r <- r + impute_dataframe_aux(df,net_cfg)
  r/30
}
impute_dataframe_aux <- function(df,net_cfg) {
  ncols <- ncol(df)
  if (is.null(ncols) || ncols == 0) return(df)
  if (is.null(nrow(df)) || nrow(df) == 0) return(df)
  df$time <- 1:nrow(df)
  df$daypart <- rep(0:(net_cfg$measurements_per_day -1),nrow(df))[1:nrow(df)]
  constant_columns <- NULL
  for (i in 1:ncol(df)) {
    colname = names(df)[i]
    if (!any(is.na(df[,colname])) && all(df[,colname] == df[1,colname]))
      constant_columns <- c(constant_columns,colname)
  }
  noms <- 'daypart'
  if ('daypart' %in% constant_columns)
    noms <- NULL
  mdf <- df[!(names(df) %in% constant_columns)]
  a_out <- amelia(mdf,
                  tol=0.1, # higher tolerance to reach conversion faster
                  ts="time",
                  lags=1:(ncol(mdf)-1-ifelse(is.null(noms),0,1)),
                  noms=noms,
                  polytime=2,
                  p2s=0, # no screen output. 1 for normal output, 2 for verbose output
                  empri=.01*nrow(mdf), # empirical ridge prior
                  autopri=1) # adjust empri automatically if desired solution
                             # was not found (see help)
  a_out_imps <- a_out$imputations
  if (is.null(a_out_imps)) return(df[,1:ncols])
  # calculate the mean imputation data set
  mean_imp <- .2 * Reduce('+',a_out_imps)
  # do not return time or daypart
  mdf <- mean_imp[,1:ncols]
  df[!(names(df) %in% constant_columns)] <- mdf
  df[,1:ncols]
}
