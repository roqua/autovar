#' Convert best model to graph
#'
#' This function returns a JSON representation of a the graphs for the best valid model found in the given \code{av_state}.
#' @param av_state an object of class \code{av_state}, containing at least one valid model
#' @return This function returns a string representing a json array of two networks.
#' @export
convert_to_graph <- function(av_state) {
  rnames <- NULL
  nodedata <- NULL
  linkdata <- NULL
  nodecount <- 0
  res <- av_state$accepted_models[[1]]$varest$varresult
  var_names <- names(res)
  i <- 0
  for (varname in var_names) {
    nodedata <- rbind(nodedata,data.frame(index=nodecount,
                                          name=format_property_name(unprefix_ln(varname)),
                                          type=format_property_type(unprefix_ln(varname)),
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
                                              name=format_property_name(unprefix_ln(varname)),
                                              type=format_property_type(unprefix_ln(varname)),
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
                                                name=format_property_name(unprefix_ln(varname)),
                                                type=format_property_type(unprefix_ln(varname)),
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
        toString(toJSON(list(links=linkdata,nodes=nodedata))),
        ',',
        toString(toJSON(list(links=linkdatac,nodes=nodedatac))),
        ']',sep="")
}

correlation_significance <- function() {
  0.05
}

format_property_name <- function(rname) {
  # TODO: rewrite this to require a net_config
  switch(rname,
         ontspanning = "Ontspanning",
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
         uw_eigen_factor = "Mijn eigen factor",
         rname)
}
format_property_type <- function(rname) {
  bal <- property_balance(rname)
  if (bal == 1) return('Positief')
  if (bal == -1) return('Negatief')
  'Neutraal'
}
