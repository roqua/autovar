#' Return a JSON array of network data of a fitting model
#'
#' This function uses repeated calls to \code{\link{var_main}} to find a fitting model for the data provided and then calls \code{\link{convert_to_graph}} to return a JSON representation of best valid model found.
#' @param data a data frame. Each row is a measurement. Each column is an endogenous variable.
#' @param timestamp the date of the first measurement in the format \code{'yyyy-mm-dd'}. If multiple measurements are taken per day, the first measurement (row) in the data frame should correspond to the first measurement on that day.
#' @param always_include a vector of variable names that should always be included in the network if possible. Can also be \code{NULL}, in which case no variables are included by default.
#' @param pairs a vector of variable names in the form \code{c('pair1a','pair1b','pair2a','pair2b','pair3a',...)}. In other words: it is a vector of even length in which each two subsequent positions are seen as a pair. The pairs are treated such that only one pair or one variable of the pairs is included in the networks.
#' @param positive_variables a vector of names of variables that measure a positive affect (e.g., happiness). Variable names not occurring in the \code{positive_variables} or \code{negative_variables} list are considered neutral.
#' @param negative_variables a vector of names of variables that measure a negative affect (e.g., sadness). Variable names not occurring in the \code{positive_variables} or \code{negative_variables} list are considered neutral.
#' @param pick_best_of a vector of variable names of which one should always be included in the network. Can also be \code{NULL} (the default value), in which case no variable is always included (unless specified by \code{always_include}).
#' @param incident_to_best_of if \code{pick_best_of} is not NULL, a choice is made between the networks generated at each iteration based on which has the most incoming edges that originate from nodes listed in the vector of variable names \code{incident_to_best_of}.
#' @param labels a list where keys are variable names and values are labels.
#' @param measurements_per_day an integer in [1,16] denoting the number of measurements per day. Defaults to 3.
#' @param max_network_size an integer in [2,6] denoting the number of nodes to include in the networks initially. Defaults to 6.
#' @param include_model determines whether the imputed data set and coefficients of the best model should be returned in the network JSON. Defaults to \code{FALSE}.
#' @param second_significances is the vector of significance levels to be used after the first attempt.
#' @return This function returns a string representing a json array of two networks and an array of the top links.
#' @examples
#' \dontrun{
#' GN_COLUMNS <- c('ontspanning', 'opgewektheid', 'hier_en_nu', 'concentratie',
#'                 'beweging', 'iets_betekenen', 'humor', 'buiten_zijn',
#'                 'eigenwaarde', 'levenslust', 'onrust', 'somberheid',
#'                 'lichamelijk_ongemak', 'tekortschieten', 'piekeren', 'eenzaamheid',
#'                 'uw_eigen_factor')
#' d<-load_file("../data/input/DataDndN_nonimputed_voorAndo.sav",log_level=3)
#' d<-d$raw_data[,GN_COLUMNS]
#' timestamp <- '2014-03-01'
#' cat(generate_networks(data = d,
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
#'                   max_network_size = 6))
#' }
#' @export
generate_networks <- function(data, timestamp, always_include = NULL, pairs = NULL, positive_variables = NULL,
                              negative_variables= NULL, pick_best_of = NULL, incident_to_best_of = NULL,
                              labels = list(), measurements_per_day = 3, max_network_size = 6,
                              include_model = FALSE, second_significances = c(0.05,0.01,0.005)) {
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
  net_cfg$pick_best_of <- unique(pick_best_of)
  net_cfg$incident_to_best_of <- unique(incident_to_best_of)
  net_cfg$labels <- labels
  net_cfg$include_model <- include_model
  if (!(measurements_per_day %in% 1:16)) return("measurements_per_day needs to be in 1:16")
  net_cfg$measurements_per_day <- measurements_per_day
  if (!(max_network_size %in% 2:6)) return("max_network_size needs to be in 2:6")
  net_cfg$max_network_size <- max_network_size
  check_res <- check_config_integrity(net_cfg)
  if (!is.null(check_res)) return(check_res)
  mycores <- parallel::detectCores()
  if (is.na(mycores))
    mycores <- 1
  for (attempt in 1:(net_cfg$max_network_size)) {
    fail_safe <- FALSE
    number_of_columns <- net_cfg$max_network_size
    if (attempt > 1) {
      fail_safe <- TRUE
      # attempt to generate networks for the initial network size (max_network_size) twice,
      # once with balancing and once without.
      number_of_columns <- net_cfg$max_network_size+2-attempt
    }
    list_of_column_configs <- list()
    if (is.null(net_cfg$pick_best_of) || is.null(net_cfg$incident_to_best_of)) {
      list_of_column_configs <- c(list_of_column_configs,list(select_relevant_columns(data,net_cfg,fail_safe,number_of_columns,log_level=3)))
    } else {
      for (idx in 1:length(net_cfg$pick_best_of)) {
        if (psych::mssd(data[,net_cfg$pick_best_of[[idx]]]) <= mssd_threshold()) {
          list_of_column_configs <- c(list_of_column_configs,list(NULL))
          next
        }
        force_include_var <- net_cfg$pick_best_of[[idx]]
        force_exclude_vars <- net_cfg$pick_best_of[net_cfg$pick_best_of != force_include_var]
        filtered_data <- data[!(names(data) %in% force_exclude_vars)]
        list_of_column_configs <- c(list_of_column_configs,list(select_relevant_columns(filtered_data,net_cfg,
                                                                                        fail_safe,
                                                                                        number_of_columns,
                                                                                        log_level=3,
                                                                                        force_include=force_include_var)))
      }
    }

    # Imputation + cutting rows part
    new_list_of_column_configs <- list()
    for (i in 1:length(list_of_column_configs)) {
      odata <- list_of_column_configs[[i]]
      if (is.null(odata)) {
        new_list_of_column_configs <- c(new_list_of_column_configs,list(NULL))
        next
      }
      first_measurement_index <- 1
      res <- select_relevant_rows(odata,timestamp,net_cfg)
      odata <- res$data
      first_measurement_index <- res$first_measurement_index
      new_timestamp <- res$timestamp
      if (any(is.na(odata)))
        odata <- impute_dataframe(odata,net_cfg$measurements_per_day)
      if (any(is.na(odata))) {
        new_list_of_column_configs <- c(new_list_of_column_configs,list(NULL))
        next # sometimes it fails
      }
      new_list_of_column_configs <- c(new_list_of_column_configs,list(list(timestamp = new_timestamp,
                                                                           first_measurement_index = first_measurement_index,
                                                                           data = odata)))
    }
    list_of_column_configs <- new_list_of_column_configs

    # Loop over significances here
    SIGNIFICANCES <- c(0.05,0.01)
    if (attempt > 1) SIGNIFICANCES <- second_significances
    for (signif in SIGNIFICANCES) {
      best_graph <- NULL
      most_incident_edges <- -1
      for (idx in 1:length(list_of_column_configs)) {
        column_config <- list_of_column_configs[[idx]]
        if (is.null(column_config)) next
        new_timestamp <- column_config$timestamp
        ndata <- column_config$data
        first_measurement_index <- column_config$first_measurement_index

        # Start Autovar procedure from here
        d<-load_dataframe(ndata,net_cfg,log_level=3)
        d<-add_trend(d,log_level=3)
        d<-set_timestamps(d,date_of_first_measurement=new_timestamp,
                          first_measurement_index=first_measurement_index,
                          measurements_per_day=net_cfg$measurements_per_day,log_level=3)
        d<-var_main(d,names(ndata),significance=signif,log_level=3,
                    criterion="AIC",include_squared_trend=TRUE,
                    exclude_almost=TRUE,simple_models=TRUE,
                    split_up_outliers=TRUE,numcores=mycores)
        if (length(d$accepted_models) > 0) {
          if (is.null(net_cfg$pick_best_of) || is.null(net_cfg$incident_to_best_of))
            return(convert_to_graph(d,net_cfg))
          current_graph <- convert_to_graph(d,net_cfg,forced_variable = net_cfg$pick_best_of[[idx]])
          current_number_of_incident_edges <- number_of_edges(current_graph,
                                                              from_nodes = label_nodes(net_cfg$incident_to_best_of,net_cfg),
                                                              to_node = label_nodes(net_cfg$pick_best_of[[idx]],net_cfg))
          if (current_number_of_incident_edges > most_incident_edges) {
            most_incident_edges <- current_number_of_incident_edges
            best_graph <- current_graph
          }
        }
      }
      if (!is.null(best_graph))
        return(best_graph)
    }
  }
  NULL
}
label_nodes <- function(node_vector, net_cfg) {
  r <- NULL
  for (node in node_vector)
    r <- c(r, format_property_name(node, net_cfg))
  r
}
number_of_edges <- function(graph, from_nodes, to_node) {
  dynamic_graph <- jsonlite::fromJSON(graph)[[1]]
  if (is.null(dynamic_graph$links) || (class(dynamic_graph$links) == 'character' && dynamic_graph$links == '')) return(0)
  to_node_index <- dynamic_graph$nodes$index[dynamic_graph$nodes$name == to_node]
  from_node_indices <- dynamic_graph$nodes$index[dynamic_graph$nodes$name %in% from_nodes]
  r <- 0
  for (idx in 1:nrow(dynamic_graph$links)) {
    cur_row <- dynamic_graph$links[idx,]
    if (cur_row$target == to_node_index && cur_row$source %in% from_node_indices)
      r <- r + 1
  }
  r
}

check_config_integrity <- function(net_cfg) {
  for (varname in net_cfg$always_include) {
    if (!(varname %in% net_cfg$vars))
      return(paste("always include var",varname,"not found in data.frame"))
    if (varname %in% net_cfg$pairs)
      return(paste("always include var",varname,"also found in pairs"))
  }
  for (varname in net_cfg$pairs)
    if (!(varname %in% net_cfg$vars))
      return(paste("pair var",varname,"not found in data.frame"))
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
  for (varname in net_cfg$pick_best_of)
    if (!(varname %in% net_cfg$vars))
      return(paste("pick_best_of variable",varname,"not found in data.frame"))
  for (varname in net_cfg$incident_to_best_of)
    if (!(varname %in% net_cfg$vars))
      return(paste("incident_to_best_of variable",varname,"not found in data.frame"))
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
                    max_network_size = 6,
                    include_model = TRUE,
                    second_significances = c(0.05,0.01,0.005))
}

generate_networks_debug <- function(...) {
  tryCatch(generate_networks(...),
           error = function(e) { e$message <- paste(e$message, "\n", paste(sys.calls(), "\n", collapse = "\n"), collapse = "\n"); stop(e) })
}

generate_networks_clean <- function(data, timestamp, always_include = NULL, pairs = NULL, positive_variables = NULL,
                                    negative_variables = NULL, pick_best_of = NULL, incident_to_best_of = NULL,
                                    labels = list(), measurements_per_day = 3, max_network_size = 6,
                                    include_model = FALSE, second_significances = c(0.05,0.01,0.005)) {
  orig_col_count <- length(colnames(data))
  column_names <- colnames(data)
  empty_columns <- NULL
  for (i in 1:orig_col_count) {
    column_name <- column_names[i]
    column_data <- data[[column_name]]
    if (all(is.na(column_data)))
      empty_columns <- c(empty_columns, column_name)
  }
  new_data <- data[column_names[!(column_names %in% empty_columns)]]
  new_always_include <- always_include[!(always_include %in% empty_columns)]
  new_pairs <- NULL
  if (!is.null(pairs)) {
    i <- 1
    while (i < length(pairs)) {
      pair_first <- pairs[i]
      pair_second <- pairs[i + 1]
      i <- i + 2
      if (pair_first %in% empty_columns || pair_second %in% empty_columns) next
      new_pairs <- c(new_pairs, pair_first, pair_second)
    }
  }
  new_positive_variables <- positive_variables[!(positive_variables %in% empty_columns)]
  new_negative_variables <- negative_variables[!(negative_variables %in% empty_columns)]
  new_pick_best_of <- pick_best_of[!(pick_best_of %in% empty_columns)]
  new_incident_to_best_of <- incident_to_best_of[!(incident_to_best_of %in% empty_columns)]
  new_labels <- labels[names(labels)[!(names(labels) %in% empty_columns)]]
  generate_networks(data = new_data,
                    timestamp = timestamp,
                    always_include = new_always_include,
                    pairs = new_pairs,
                    positive_variables = new_positive_variables,
                    negative_variables = new_negative_variables,
                    pick_best_of = new_pick_best_of,
                    incident_to_best_of = new_incident_to_best_of,
                    labels = new_labels,
                    measurements_per_day = measurements_per_day,
                    max_network_size = max_network_size,
                    include_model = include_model,
                    second_significances = second_significances)
}
