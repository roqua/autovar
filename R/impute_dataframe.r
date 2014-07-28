#' Impute missing values in a data.frame using EM imputation
#'
#' This function uses the Amelia package to impute a given data frame and return the imputed values.
#' @param df a data frame
#' @param polytime integer between 0 and 3 indicating what power of polynomial should be included in the imputation model to account for the effects of time. A setting of 0 would indicate constant levels, 1 would indicate linear time effects, 2 would indicate squared effects, and 3 would indicate cubic time effects.
#' @return This function returns the modified data frame.
#' @export
impute_dataframe <- function(df,polytime) {
  r <- impute_dataframe_aux(df,polytime)
  for (i in 1:29)
    r <- r + impute_dataframe_aux(df,polytime)
  r/30
}
impute_dataframe_aux <- function(df,polytime) {
  ncols <- ncol(df)
  df$time <- 1:nrow(df)
  df$daypart <- rep(c(0,1,2),30)[1:nrow(df)]
  a_out <- amelia(df,
                  tol=0.1, # higher tolerance to reach conversion faster
                  ts="time",
                  lags=1:ncols,
                  noms="daypart",
                  polytime=polytime,
                  p2s=0, # no screen output. 1 for normal output, 2 for verbose output
                  empri=.01*nrow(df), # empirical ridge prior
                  autopri=1) # adjust empri automatically if desired solution
                             # was not found (see help)
  a_out_imps <- a_out$imputations
  # calculate the mean imputation data set
  mean_imp <- .2 * Reduce('+',a_out_imps)
  # do not return time or daypart
  mean_imp[,1:ncols]
}
