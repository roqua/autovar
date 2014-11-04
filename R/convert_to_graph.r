#' Convert best model to graph
#'
#' This function returns a JSON representation of a the graphs for the best valid model found in the given \code{av_state}.
#' @param av_state an object of class \code{av_state}, containing at least one valid model
#' @param net_cfg a net_cfg object providing metadata about the networks
#' @return This function returns a string representing a json array of two networks.
#' @examples
#' GN_COLUMNS <- c('ontspanning', 'opgewektheid', 'hier_en_nu', 'concentratie',
#'                 'beweging', 'iets_betekenen', 'humor', 'buiten_zijn',
#'                 'eigenwaarde', 'levenslust', 'onrust', 'somberheid',
#'                 'lichamelijk_ongemak', 'tekortschieten', 'piekeren', 'eenzaamheid',
#'                 'uw_eigen_factor')
#' data<-load_file("../data/input/DataDndN_nonimputed_voorAndo.sav",log_level=3)
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
#' net_cfg$measurements_per_day <- 3
#' net_cfg$max_network_size <- 6
#' odata <- select_relevant_columns(data,net_cfg,FALSE,6)
#' first_measurement_index <- 1
#' timestamp <- '2014-05-06'
#' res <- select_relevant_rows(odata,timestamp,net_cfg)
#' odata <- res$data
#' first_measurement_index <- res$first_measurement_index
#' timestamp <- res$timestamp
#' if (any(is.na(odata)))
#'   odata <- impute_dataframe(odata,net_cfg)
#' d<-load_dataframe(odata,net_cfg)
#' d<-add_trend(d)
#' d<-set_timestamps(d,date_of_first_measurement=timestamp,
#'                     first_measurement_index=first_measurement_index,
#'                     measurements_per_day=net_cfg$measurements_per_day)
#' d<-var_main(d,names(odata),significance=0.01,log_level=3,
#'               criterion="AIC",include_squared_trend=TRUE,
#'               exclude_almost=TRUE,simple_models=TRUE,
#'               split_up_outliers=TRUE)
#' cat(convert_to_graph(d,net_cfg))
#' @export
convert_to_graph <- function(av_state,net_cfg) {
  rnames <- NULL
  nodedata <- NULL
  linkdata <- NULL
  nodecount <- 0
  res <- av_state$accepted_models[[1]]$varest$varresult
  var_names <- names(res)
  i <- 0
  for (varname in var_names) {
    nodedata <- rbind(nodedata,data.frame(index=nodecount,
                                          name=format_property_name(unprefix_ln(varname),net_cfg),
                                          type=format_property_type(unprefix_ln(varname),net_cfg),
                                          stringsAsFactors=FALSE))
    rnames <- c(rnames,varname)
    nodecount <- nodecount+1
  }
  nodedatac <- nodedata
  nodecountc <- nodecount
  rnamesc <- rnames
  for (equation in res) {
    i <- i+1
    eqsum <- summary(equation)
    eqname <- var_names[i]
    for (fromnodename in var_names) {
      if (fromnodename == eqname) next
      p_val <- eqsum$coefficients[paste(fromnodename,'.l1',sep=""),4]
      if (p_val > 0.05) next
      coef <- eqsum$coefficients[paste(fromnodename,'.l1',sep=""),1]
      for (varname in c(eqname,fromnodename)) {
        if (varname %in% rnames) next
        nodedata <- rbind(nodedata,data.frame(index=nodecount,
                                              name=format_property_name(unprefix_ln(varname),net_cfg),
                                              type=format_property_type(unprefix_ln(varname),net_cfg),
                                              stringsAsFactors=FALSE))
        rnames <- c(rnames,varname)
        nodecount <- nodecount+1
      }
      tonode<- which(eqname == rnames) -1
      fromnode <- which(fromnodename == rnames) -1
      linkdata <- rbind(linkdata,data.frame(source=fromnode,
                                            target=tonode,
                                            coef=toString(coef),
                                            stringsAsFactors=FALSE))
    }
  }
  # contemp eqs
  linkdatac <- NULL
  signmat <- significance_matrix(summary(av_state$accepted_models[[1]]$varest))
  n <- length(var_names)
  for (i in 1:(n-1))
    for (j in (i+1):n) {
      if (signmat[j*2,i] > correlation_significance() || signmat[j*2-1,i] == 0) next
      for (varname in c(var_names[[i]],var_names[[j]])) {
        if (varname %in% rnamesc) next
        nodedatac <- rbind(nodedatac,data.frame(index=nodecountc,
                                                name=format_property_name(unprefix_ln(varname),net_cfg),
                                                type=format_property_type(unprefix_ln(varname),net_cfg),
                                                stringsAsFactors=FALSE))
        rnamesc <- c(rnamesc,varname)
        nodecountc <- nodecountc+1
      }
      tonode   <- which(var_names[[i]] == rnamesc) -1
      fromnode <- which(var_names[[j]] == rnamesc) -1
      linkdatac <- rbind(linkdatac,data.frame(source=fromnode,
                                              target=tonode,
                                              coef=toString(signmat[j*2-1,i]),
                                              stringsAsFactors=FALSE))
    }
  paste('[',
        toString(toJSON(list(links=linkdata,nodes=nodedata))),     # dynamic
        ',',
        toString(toJSON(list(links=linkdatac,nodes=nodedatac))),   # contemporaneous
        ']',sep="")
}

correlation_significance <- function() {
  0.05
}

format_property_name <- function(rname,net_cfg) {
  label <- net_cfg$labels[[rname]]
  if (!is.null(label)) return(label)
  rname
}
format_property_type <- function(rname,net_cfg) {
  bal <- property_balance(rname,net_cfg)
  if (bal == 1) return('Positief')
  if (bal == -1) return('Negatief')
  'Neutraal'
}
