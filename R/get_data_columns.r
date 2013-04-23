get_data_columns <- function(av_state,model) {
  exodta <- NULL
  endo <- NULL
  model_valid <- TRUE
  
  vrs <- av_state$vars
  
  # check if log transform needs to be applied
  if (apply_log_transform(model)) {
    for (name in vrs) {
      ln_name <- prefix_ln(name)
      if (!column_exists(av_state,ln_name)) {
        av_state <- add_derived_column(av_state,
                                       ln_name,
                                       name,
                                       operation='LN',
                                       log_level=av_state$log_level)
      }
    }
    vrs <- sapply(vrs,prefix_ln,USE.NAMES=FALSE)
  }
  endo <- av_state$data[[av_state$subset]][vrs]
  
  # check if exogenous_variables need to be created
  if (!is.null(model$exogenous_variables)) {
    gor <- get_orig_resids(model,av_state)
    av_state <- gor$av_state
    orig_resids <- gor$resids
    nr_obs <- dim(av_state$data[[av_state$subset]])[[1]]
    if (!is.null(orig_resids)) {
      exovrs <- NULL
      for (i in 1:nr_rows(model$exogenous_variables)) {
        exovar <- model$exogenous_variables[i,]
        cname <- prefix_ln_cond(exovar$variable,model)
        cresids <- orig_resids[,cname]
        exovr <- paste(exovar$variable,'_outliers',sep='')
        av_state$data[[av_state$subset]][[exovr]] <-
          get_outliers_column(cresids,exovar$iteration,nr_obs)
        if (!is_outliers_column_valid(cresids,
                                      exovar$iteration,
                                      cname,
                                      nr_obs,
                                      av_state$log_level)) {
          model_valid <- FALSE
        }
        exovrs <- c(exovrs,exovr)
      }
      remaining_exogenous_variables <- remaining_exogenous_variables(av_state,model)
      if (!is.null(remaining_exogenous_variables)) {
        exovrs <- c(exovrs,remaining_exogenous_variables)
      }
      exodta <- av_state$data[[av_state$subset]][exovrs]
      exodta <- as.matrix(exodta)
      colnames(exodta) <- exovrs
    }
  } else if (!is.null(remaining_exogenous_variables(av_state,model))) {
    exovrs <- remaining_exogenous_variables(av_state,model)
    exodta <- av_state$data[[av_state$subset]][exovrs]
    exodta <- as.matrix(exodta)
    colnames(exodta) <- exovrs
    if (!is.null(exodta) && dim(exodta)[[2]] == 0) {
      exodta <- NULL
    }
  }
  list(av_state = av_state,endogenous = endo, 
       exogenous = exodta, model_valid = model_valid)
}

remaining_exogenous_variables <- function(av_state,model) {
  exovrs <- av_state$exogenous_variables
  if (!is.null(model$include_day_dummies) && model$include_day_dummies) {
    exovrs <- unique(c(exovrs,av_state$day_dummies))
  }
  if (!is.null(model$include_trend_vars) && model$include_trend_vars) {
    exovrs <- unique(c(exovrs,av_state$trend_vars))
  }
  exovrs
}

get_endodta <- function(model,av_state) {
  vrs <- av_state$vars
  if (apply_log_transform(model)) {
    vrs <- sapply(vrs,prefix_ln,USE.NAMES=FALSE)
  }
  av_state$data[[av_state$subset]][vrs]
}

get_orig_resids <- function(model,av_state) {
  resids <- NULL
  if (!is.null(model$lag) && model$lag != -1) {
    lstname <- ifelse(apply_log_transform(model),'log_resids','resids')
    #if (model$lag > length(av_state[[lstname]]) || is.null(av_state[[lstname]][[model$lag]])) {
      # if residuals are not cached yet, cache them
      #cat("get_orig_resids: caching for: log_transform:",apply_log_transform(model),
      #    "lag:",model$lag,"\n")
      endodta <- get_endodta(model,av_state)
      exodta <- NULL
      if (!is.null(remaining_exogenous_variables(av_state,model))) {
        exovrs <- remaining_exogenous_variables(av_state,model)
        exodta <- av_state$data[[av_state$subset]][exovrs]
        exodta <- as.matrix(exodta)
        colnames(exodta) <- exovrs
        if (!is.null(exodta) && dim(exodta)[[2]] == 0) {
          exodta <- NULL
        }
      }
      varest <- run_var(data = endodta, lag = model$lag, exogen = exodta)
      av_state[[lstname]][[model$lag]] <- resid(varest)
    #}
    resids <- av_state[[lstname]][[model$lag]]
  }
  list(av_state = av_state,resids = resids)
}

store_residuals <- function(av_state,model,resids) {
  lstname <- ifelse(apply_log_transform(model),'log_resids','resids')
  if (model$lag > length(av_state[[lstname]]) || is.null(av_state[[lstname]][[model$lag]])) {
    #cat("store_residuals: caching for: log_transform:",apply_log_transform(model),
    #      "lag:",model$lag,"\n")
    av_state[[lstname]][[model$lag]] <- resids
  }
  av_state
}

apply_log_transform <- function(model) {
  !is.null(model$apply_log_transform) && model$apply_log_transform
}

is_restricted_model <- function(model) {
  !is.null(model$restrict) && model$restrict
}

nr_rows <- function(df) {
  dim(df)[1]
}

column_exists <- function(av_state,name) {
  any(name == names(av_state$data[[av_state$subset]]))
}

prefix_ln_cond <- function(str,model) {
  if (apply_log_transform(model)) {
    prefix_ln(str)
  } else {
    str
  }
}

prefix_ln <- function(str) {
  paste('ln',str,sep="")
}

unprefix_ln <- function(str) {
  sub("^ln",'',str)
}

get_outliers_column <- function(dta,iteration,nr_obs) {
  std <- sd(dta)
  mu <- mean(dta)
  std_factor <- std_factor_for_iteration(iteration)
  res <- unname(((dta < mu-std_factor*std) | (dta > mu+std_factor*std))+0)
  if (iteration == 3) {
    dta <- dta*dta
    std <- sd(dta)
    mu <- mean(dta)
    res2 <- unname(((dta < mu-std_factor*std) | (dta > mu+std_factor*std))+0)
    res <- pmax(res,res2)
  }
  if (length(res) < nr_obs) {
    res <- c(rep.int(0,nr_obs-length(res)),res)
  }
  res
}

is_outliers_column_valid <- function(dta,iteration,cname,nr_obs,log_level) {
  model_valid <- TRUE
  outliers_column <- get_outliers_column(dta,iteration,nr_obs)
  if ((iteration == 1 && all(outliers_column == 0)) ||
        (iteration > 1 && all(outliers_column == get_outliers_column(dta,iteration-1,nr_obs)))) {
    std_factor <- std_factor_for_iteration(iteration)
    if (iteration == 1) {
      scat(log_level,2,"\n> Removing ",std_factor,"x std. outliers for ",
           cname," residuals has no effect. Marking model as invalid.\n",sep='')
    } else {
      scat(log_level,2,"\n> Removing ",std_factor,"x std. outliers for ",
           cname," residuals has the same effect as removing ",
           std_factor_for_iteration(iteration-1),
           "x std. outliers. Marking model as invalid.\n",sep='')
    }
    model_valid <- FALSE
  }
  model_valid
}

std_factor_for_iteration <- function(iteration) {
  std_factor <- NULL
  if (iteration == 1) {
    # remove outliers beyond 3.5xstd
    std_factor <- 3.5
  } else if (iteration == 2) {
    # remove outliers beyond 3x std
    std_factor <- 3
  } else {
    # remove outliers beyond 2.5 std
    std_factor <- 2.5
  }
  std_factor
}

get_outliers_as_string <- function(av_state,name,iteration,model) {
  orig_resids <- get_orig_resids(model,av_state)$resids
  if (is.null(orig_resids)) {
    '???'
  } else {
    cname <- prefix_ln_cond(name,model)
    cresids <- orig_resids[,cname]
    column <- get_outliers_column(cresids,iteration,
                                  dim(av_state$data[[av_state$subset]])[[1]])
    paste(which(column == 1),collapse=', ')
  }
}

get_outliers_as_vector <- function(av_state,name,iteration,model) {
  orig_resids <- get_orig_resids(model,av_state)$resids
  if (is.null(orig_resids)) {
    NULL
  } else {
    cname <- prefix_ln_cond(name,model)
    cresids <- orig_resids[,cname]
    column <- get_outliers_column(cresids,iteration,
                                  dim(av_state$data[[av_state$subset]])[[1]])
    which(column == 1)
  }
}
