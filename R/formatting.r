# A more user friendly layout for printing just the accepted models.
# This will also list exogenous variables specified for all models
format_accepted_models <- function(av_state) {
  idx <- 0
  res <- ""
  for (x in av_state$accepted_models) {
    idx <- idx+1
    char <- idx_chars(idx)
    res <- paste(res,char,": ",
                 printed_model_score(x$varest),
                 " : ",
                 vargranger_line(x$varest),
                 "\n",
                 sep='')
    # Lag: 2
    res <- paste(res,"  Lag: ",x$parameters$lag,"\n",sep='')
    # Log transform: YES
    res <- paste(res,"  Log transform: ",
                 ifelse(apply_log_transform(x$parameters),"YES","NO")
                 ,"\n",sep='')
    # Exogenous variables:
    res <- paste(res,"  Exogenous variables: ",sep='')
    res <- paste(res,
                 format_exogenous_variables(x$parameters$exogenous_variables,
                                            av_state,
                                            x$parameters,x$varest),
                 sep='')
    # Constraints:
    res <- paste(res,"  Constraints: ",sep='')
    res <- paste(res,
                 format_constraints(x$varest,av_state$exogenous_variables))
    res <- paste(res,"\n",sep='')
    #if (idx == 3) { break }
  }
  res
}
idx_chars <- function(idx) {
  chars <- LETTERS[[((idx-1)%%26)+1]]
  if ((idx-1)%/%26 > 0) {
    chars <- paste(LETTERS[[(idx-1)%/%26]],chars,sep='')
  }
  chars
}
format_exogenous_variables <- function(exogvars,av_state,model,varest) {
  remaining_exog_vars <- remaining_exogenous_variables(av_state,model)
  remaining_exog_vars <- remove_restricted_variables(remaining_exog_vars,varest)
  if (is.null(exogvars) && is.null(remaining_exog_vars)) {
    "none\n"
  } else {
    res <- "\n"
    if (!is.null(remaining_exog_vars)) {
      for (i in 1:length(remaining_exog_vars)) {
        exovar <- remaining_exog_vars[[i]]
        res <- paste(res,'    ',exovar,': ',
                     paste(which(av_state$data[[av_state$subset]][[exovar]] == 1),collapse=', '),
                     '\n',sep='')
      }
    }
    if (!is.null(exogvars)) {
      for (i in 1:nr_rows(exogvars)) {
        exovar <- exogvars[i,]
        desc <- format_varname(exovar$variable)
        std_factor <- std_factor_for_iteration(exovar$iteration)
        res <- paste(res,'    ',desc,' (',std_factor,'x std. of res.): ',
                     get_outliers_as_string(av_state,exovar$variable,
                                            exovar$iteration,
                                            model),
                     "\n",
                     sep='')
      }
    }
    res
  }
}
remove_restricted_variables <- function(exog_vars,varest) {
  if (!is.null(varest$restrictions)) {
    new_exog_vars <- NULL
    for (varname in exog_vars) {
      if (!all(varest$restrictions[,which(dimnames(varest$restrictions)[[2]] == varname)] == 0)) {
        new_exog_vars <- c(new_exog_vars,varname)
      }
    }
    new_exog_vars
  } else {
    exog_vars
  }
}
format_varname <- function(varname) {
  paste(varname,' outliers',sep='')
}
format_constraints <- function(varest,exogvars) {
  if (is.null(varest$restrictions)) {
    "none\n"
  } else {
    paste(restrictions_tostring(varest,exogvars),"\n",sep='')
  }
}


