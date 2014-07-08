#' Return a JSON array of network data of a fitting model
#' 
#' This function uses repeated calls to \code{\link{var_main}} to find a fitting model for the data provided and then calls \code{\link{convert_to_graph}} to return a JSON representation of best valid model found.
#' @param data a data frame of 17 columns (ontspanning, opgewektheid, hier_en_nu, concentratie, beweging, iets_betekenen, humor, buiten_zijn, eigenwaarde, levenslust, onrust, somberheid, lichamelijk_ongemak, tekortschieten, piekeren, eenzaamheid, uw_eigen_factor) and 90 rows
#' @param timestamp the date of the first measurement in the format \code{'yyyy-mm-dd'}
#' @return This function returns a string representing a json array of two networks.
#' @export
generate_network <- function(data, timestamp) {
  # TODO: impute data not implemented
  # data <- impute_data(data,timestamp) # oid
  return("Franque")
  data <- select_relevant_columns(data)
  SIGNIFICANCES <- c(0.05,0.01,0.005,0.001)
  for (signif in SIGNIFICANCES) {
    d<-load_dataframe(data,log_level=3)
    d<-add_trend(d,log_level=3)
    d<-set_timestamps(d,date_of_first_measurement=timestamp,
                      measurements_per_day=3,log_level=3)
    # squared trend is always included when trend is
    d<-var_main(d,names(data),lag_max=1,significance=signif,
                exogenous_max_iterations=1,log_level=3,
                criterion="AIC",include_squared_trend=TRUE,
                exclude_almost=TRUE,simple_models=TRUE)
    if (length(d$accepted_models) > 0)
      return(convert_to_graph(d))
    # 8 modellen
    # av_state$simple_models means: do not look for constraints AND add those special ones at the start
  }
  NULL
}
