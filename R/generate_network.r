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
  
  # select relevant columns (reduce number of columns to 6 max.)
  data <- select_relevant_columns(data)
  return(data)
  SIGNIFICANCES <- c(0.05,0.01,0.005,0.001)
  for (significance in SIGNIFICANCES) {
    # run something
    # if there are valid models
    # return convert_to_graph(av_state)
  }
  NULL
}
