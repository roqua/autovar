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
                                            x$parameters),
                 sep='')
    # Constraints:
    res <- paste(res,"  Constraints: ",sep='')
    res <- paste(res,
                 format_constraints(x$varest))
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
format_exogenous_variables <- function(exogvars,av_state,model) {
  if (is.null(exogvars) && is.null(av_state$exogenous_variables)) {
    "none\n"
  } else {
    res <- "\n"
    if (!is.null(av_state$exogenous_variables)) {
      for (i in 1:length(av_state$exogenous_variables)) {
        exovar <- av_state$exogenous_variables[[i]]
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
format_varname <- function(varname) {
  paste(varname,' outliers',sep='')
}
format_constraints <- function(varest) {
  if (is.null(varest$restrictions)) {
    "none\n"
  } else {
    paste(restrictions_tostring(varest),"\n",sep='')
  }
}


