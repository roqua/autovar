#' Impute missing values in a data.frame using EM imputation
#'
#' This function uses the Amelia package to impute a given data frame and return the imputed values.
#' @param df a data frame
#' @param measurements_per_day The number of measurements per day in the diary study
#' @param repetitions The amount of times the Amelia call should be averaged over. Defaults to 30. The actual number of imputations is five times the value for \code{repetitions}, since Amelia's values are already averaged over five runs.
#' @return This function returns the modified data frame.
#' @examples
#' data <- generate_numerical_test_data(40)
#' data
#' impute_dataframe(data,measurements_per_day=1)
#' @export
impute_dataframe <- function(df,measurements_per_day,repetitions=30) {
  if (repetitions < 1) stop("repetitions has to be at least 1")
  r <- impute_dataframe_aux(df,measurements_per_day)
  if (repetitions > 1) {
    for (i in 1:(repetitions-1))
      r <- r + impute_dataframe_aux(df,measurements_per_day)
  }
  r/repetitions
}
impute_dataframe_aux <- function(df,measurements_per_day) {
  ncols <- ncol(df)
  if (is.null(ncols) || ncols == 0) return(df)
  if (is.null(nrow(df)) || nrow(df) == 0) return(df)
  df$time <- 1:nrow(df)
  df$daypart <- rep(0:(measurements_per_day -1),nrow(df))[1:nrow(df)]
  constant_columns <- NULL
  for (i in 1:ncol(df)) {
    colname = names(df)[i]    
    if (all(is.na(df[,colname])) || var(df[,colname], na.rm = TRUE) == 0) {
      constant_columns <- c(constant_columns,colname)
      if (!is.na(mean(df[,colname], na.rm = TRUE)))
        df[,colname] <- mean(df[,colname], na.rm = TRUE)
    }
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
