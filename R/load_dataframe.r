#' Returns an av_state for data loaded from a data.frame
#' 
#' This function constructs an object of class \code{av_state} based on a given \code{data.frame}.
#' @param df a \code{data.frame} with features in columns and measurements in rows.
#' @param log_level sets the minimum level of output that should be shown (a number between 0 and 3). A lower level means more verbosity. Specify a log_level of 3 to hide messages about the data set that was loaded.
#' @return This function returns the newly created \code{av_state} object.
#' @examples
#' av_state <- load_dataframe(generate_test_data())
#' print(av_state)
#' @export
load_dataframe <- function(df,log_level=0) {
  av_state <- new_av_state()
  av_state$file_name <- 'fake_filename.sav'
  av_state$real_file_name <- 'fake_filename.sav'
  av_state$file_type <- 'SPSS'
  av_state$raw_data <- df
  av_state$data <- list('multiple'=av_state$raw_data)
  scat(log_level,2,"load_dataframe loaded data.frame with",length(av_state$data[[1]]),"columns:\n")
  sprint(log_level,2,column_names_output(av_state))
  av_state
}
