#' Return a JSON array of network data of a fitting model
#'
#' This function uses repeated calls to \code{\link{var_main}} to find a fitting model for the data provided and then calls \code{\link{convert_to_graph}} to return a JSON representation of best valid model found.
#' @param data a data frame. Each row is a measurement. Each column is an endogenous variable.
#' @param timestamp the date of the first measurement in the format \code{'yyyy-mm-dd'}. If multiple measurements are taken per day, the first measurement (row) in the data frame should correspond to the first measurement on that day.
#' @param always_include a vector of variable names that should always be included in the network if possible. Can also be \code{NULL}, in which case no variables are included by default.
#' @param pairs a vector of variable names in the form \code{c('pair1a','pair1b','pair2a','pair2b','pair3a',...)}. In other words: it is a vector of even length in which each two subsequent positions are seen as a pair. The pairs are treated such that only one pair or one variable of the pairs is included in the networks.
#' @param positive_variables a vector of names of variables that measure a positive effect (e.g., happiness). Variable names not occurring in the \code{positive_variables} or \code{negative_variables} list are considered neutral.
#' @param negative_variables a vector of names of variables that measure a negative effect (e.g., sadness). Variable names not occurring in the \code{positive_variables} or \code{negative_variables} list are considered neutral.
#' @param labels a list where keys are variable names and values are labels.
#' @param measurements_per_day an integer in [1,16] denoting the number of measurements per day. Defaults to 3.
#' @param max_network_size an integer in [2,6] denoting the number of nodes to include in the networks initially. Defaults to 6.
#' @return This function returns a string representing a json array of two networks.
#' @examples
#' GN_COLUMNS <- c('ontspanning', 'opgewektheid', 'hier_en_nu', 'concentratie',
#'                 'beweging', 'iets_betekenen', 'humor', 'buiten_zijn',
#'                 'eigenwaarde', 'levenslust', 'onrust', 'somberheid',
#'                 'lichamelijk_ongemak', 'tekortschieten', 'piekeren', 'eenzaamheid',
#'                 'uw_eigen_factor')
#' d<-load_file("../data/input/DataDndN_nonimputed_voorAndo.sav",log_level=3)
#' d<-d$raw_data[,GN_COLUMNS]
#' timestamp <- '2014-03-01'
#' generate_networks(data = data,
#'                   timestamp = timestamp,
#'                   always_include = 'uw_eigen_factor',
#'                   pairs = c('opgewektheid','onrust',
#'                             'somberheid','ontspanning',
#'                             'somberheid','onrust'),
#'                   positive_variables = c('opgewektheid','ontspanning','hier_en_nu',
#'                                          'concentratie', 'beweging','iets_betekenen',
#'                                          'humor', 'buiten_zijn','eigenwaarde', 'levenslust'),
#'                   negative_variables = c('onrust','somberheid','lichamelijk_ongemak',
#'                                          'tekortschieten','piekeren','eenzaamheid'),
#'                   labels = list(ontspanning = "Ontspanning",
#'                                 opgewektheid = "Opgewektheid",
#'                                 hier_en_nu = "In het hier en nu leven",
#'                                 concentratie = "Concentratie",
#'                                 beweging = "Beweging",
#'                                 iets_betekenen = "Iets betekenen",
#'                                 humor = "Humor",
#'                                 buiten_zijn = "Buiten zijn",
#'                                 eigenwaarde = "Eigenwaarde",
#'                                 levenslust = "Levenslust",
#'                                 onrust = "Onrust",
#'                                 somberheid = "Somberheid",
#'                                 lichamelijk_ongemak = "Lichamelijk ongemak",
#'                                 tekortschieten = "Tekortschieten",
#'                                 piekeren = "Piekeren",
#'                                 eenzaamheid = "Eenzaamheid",
#'                                 uw_eigen_factor = "Mijn eigen factor"),
#'                   measurements_per_day = 3,
#'                   max_network_size = 6)
#' @export
generate_networks <- function(data, timestamp, always_include = NULL, pairs = NULL, positive_variables = NULL,
                              negative_variables= NULL, labels = list(), measurements_per_day = 3, max_network_size = 6) {
  if (class(data) != "data.frame") return("Data argument is not a data.frame")
  if (class(timestamp) != "character") return("Timestamp argument is not a character string")
  if (nchar(timestamp) != 10) return("Wrong timestamp format, should be: yyyy-mm-dd")
  net_cfg <- new_net_cfg()
  net_cfg$vars <- unique(names(data))
  net_cfg$timestamp <- timestamp
  net_cfg$always_include <- always_include
  if (length(pairs) %% 2 != 0) return("Vector of pairs should have even length")
  net_cfg$pairs <- pairs
  net_cfg$positive_variables <- unique(positive_variables)
  net_cfg$negative_variables <- unique(negative_variables)
  net_cfg$labels <- labels
  if (!(measurements_per_day %in% 1:16)) return("measurements_per_day needs to be in 1:16")
  net_cfg$measurements_per_day <- measurements_per_day
  if (!(max_network_size %in% 2:6)) return("max_network_size needs to be in 2:6")
  net_cfg$max_network_size <- max_network_size
  check_res <- check_config_integrity(net_cfg)
  if (!is.null(check_res)) return(check_res)
  for (attempt in 1:(net_cfg$max_network_size)) {
    fail_safe <- FALSE
    number_of_columns <- net_cfg$max_network_size
    if (attempt > 1) {
      fail_safe <- TRUE
      # attempt to generate networks for the initial network size (max_network_size) twice,
      # once with balancing and once without.
      number_of_columns <- net_cfg$max_network_size+2-attempt
    }
    odata <- select_relevant_columns(data,net_cfg,fail_safe,number_of_columns,log_level=3)
    if (is.null(odata)) next
    first_measurement_index <- 1
    res <- select_relevant_rows(odata,timestamp,net_cfg)
    odata <- res$data
    first_measurement_index <- res$first_measurement_index
    timestamp <- res$timestamp
    imin <- 0
    imax <- 0
    if (any(is.na(odata))) {
      imin <- 1
      imax <- 1
    }
    SIGNIFICANCES <- c(0.05,0.01)
    if (attempt > 1) SIGNIFICANCES <- c(0.05,0.01,0.005)
    for (signif in SIGNIFICANCES) {
      for (ptime in imin:imax) {
        ndata <- odata
        if (ptime == 1) ndata <- impute_dataframe(odata,2,net_cfg)
        if (ptime == 2) ndata <- impute_dataframe(odata,1,net_cfg)
        if (any(is.na(ndata))) next # sometimes it fails
        d<-load_dataframe(ndata,net_cfg,log_level=3)
        d<-add_trend(d,log_level=3)
        d<-set_timestamps(d,date_of_first_measurement=timestamp,
                          first_measurement_index=first_measurement_index,
                          measurements_per_day=net_cfg$measurements_per_day,log_level=3)
        d<-var_main(d,names(ndata),significance=signif,log_level=3,
                    criterion="AIC",include_squared_trend=TRUE,
                    exclude_almost=TRUE,simple_models=TRUE,
                    split_up_outliers=TRUE)
        gn <<- d
        if (length(d$accepted_models) > 0)
          return(convert_to_graph(d,net_cfg))
      }
    }
  }
  NULL
}

check_config_integrity <- function(net_cfg) {
  for (varname in net_cfg$always_include) {
    if (!(varname %in% net_cfg$vars))
      return(paste("always include var",varname,"not found in data.frame"))
    if (varname %in% net_cfg$pairs)
      return(paste("always include var",varname,"also found in pairs"))
  }
  for (varname in net_cfg$pairs) {
    if (!(varname %in% net_cfg$vars))
      return(paste("pair var",varname,"not found in data.frame"))
    if (varname %in% net_cfg$always_include)
      return(paste("pair var",varname,"also found in always include"))
  }
  if (!is.null(net_cfg$pairs))
    for (i in seq(1,length(net_cfg$pairs),2))
      if (net_cfg$pairs[i] == net_cfg$pairs[i+1])
        return(paste("variable",net_cfg$pairs[i],"cannot pair with itself"))
  for (varname in net_cfg$positive_variables)
    if (!(varname %in% net_cfg$vars))
      return(paste("positive variable",varname,"not found in data.frame"))
  for (varname in net_cfg$negative_variables)
    if (!(varname %in% net_cfg$vars))
      return(paste("negative variable",varname,"not found in data.frame"))
  for (varname in net_cfg$vars)
    if (varname %in% net_cfg$positive_variables && varname %in% net_cfg$negative_variables)
      return(paste("variable",varname,"cannot be both positive and negative"))
  for (varname in names(net_cfg$labels))
    if (!(varname %in% net_cfg$vars))
      return(paste("trying to specify label for unknown variable",varname))
  NULL
}

new_net_cfg <- function() {
  x <- list()
  class(x) <- 'net_cfg'
  x
}

#' Return a JSON array of network data of a fitting model specific to hoegekis.nl
#'
#' This function uses repeated calls to \code{\link{var_main}} to find a fitting model for the data provided and then calls \code{\link{convert_to_graph}} to return a JSON representation of best valid model found.
#' @param data a data frame of 17 columns (ontspanning, opgewektheid, hier_en_nu, concentratie, beweging, iets_betekenen, humor, buiten_zijn, eigenwaarde, levenslust, onrust, somberheid, lichamelijk_ongemak, tekortschieten, piekeren, eenzaamheid, uw_eigen_factor) and 90 rows
#' @param timestamp the date of the first measurement in the format \code{'yyyy-mm-dd'}
#' @return This function returns a string representing a json array of two networks.
#' @export
generate_network <- function(data, timestamp) {
  if (any(dim(data) != c(90,17))) return("Wrong number of columns or rows in the data.frame")
  generate_networks(data = data,
                    timestamp = timestamp,
                    always_include = 'uw_eigen_factor',
                    pairs = c('opgewektheid','onrust',
                              'somberheid','ontspanning',
                              'somberheid','onrust'),
                    positive_variables = c('opgewektheid','ontspanning','hier_en_nu',
                                           'concentratie', 'beweging','iets_betekenen',
                                           'humor', 'buiten_zijn','eigenwaarde', 'levenslust'),
                    negative_variables = c('onrust','somberheid','lichamelijk_ongemak',
                                           'tekortschieten','piekeren','eenzaamheid'),
                    labels = list(ontspanning = "Ontspanning",
                                  opgewektheid = "Opgewektheid",
                                  hier_en_nu = "In het hier en nu leven",
                                  concentratie = "Concentratie",
                                  beweging = "Beweging",
                                  iets_betekenen = "Iets betekenen",
                                  humor = "Humor",
                                  buiten_zijn = "Buiten zijn",
                                  eigenwaarde = "Eigenwaarde",
                                  levenslust = "Levenslust",
                                  onrust = "Onrust",
                                  somberheid = "Somberheid",
                                  lichamelijk_ongemak = "Lichamelijk ongemak",
                                  tekortschieten = "Tekortschieten",
                                  piekeren = "Piekeren",
                                  eenzaamheid = "Eenzaamheid",
                                  uw_eigen_factor = "Mijn eigen factor"),
                    measurements_per_day = 3,
                    max_network_size = 6)
}
