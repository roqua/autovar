# get_data_columns

get_data_columns <- function(model) {
  exodta <- NULL
  endo <- NULL
  
  vrs <- av_state$vars
  
  # check if log transform needs to be applied
  if (apply_log_transform(model)) {
    for (name in vrs) {
      ln_name <- prefix_ln(name)
      if (!column_exists(ln_name)) {
        add_derived_column(ln_name,name,operation='LN')
      }
    }
    vrs <- sapply(vrs,prefix_ln,USE.NAMES=FALSE)
  }
  endo <- av_state$data[[av_state$subset]][vrs]
  
  # check if exogenous_variables need to be created
  if (!is.null(model$exogenous_variables)) {
    exovrs <- NULL
    for (i in 1:nr_rows(model$exogenous_variables)) {
      exovar <- model$exogenous_variables[i,]
      cname <- ifelse(apply_log_transform(model),
                      prefix_ln(exovar$variable),
                      exovar$variable)
      exovr <- get_outliers_column(cname,iteration=exovar$iteration)
      exovrs <- c(exovrs,exovr)
    }
    exodta <- av_state$data[[av_state$subset]][exovrs]
    exodta <- as.matrix(exodta)
    colnames(exodta) <- exovrs
  }

  list(endogenous = endo, exogenous = exodta)
}

apply_log_transform <- function(model) {
  !is.null(model$apply_log_transform) && model$apply_log_transform
}

nr_rows <- function(df) {
  dim(df)[1]
}

column_exists <- function(name) {
  any(name == names(av_state$data[[av_state$subset]]))
}

prefix_ln <- function(str) {
  paste('ln',str,sep="")
}

unprefix_ln <- function(str) {
  sub("^ln",'',str)
}


get_outliers_column <- function(cname, iteration) {
  target_column <- paste(cname,'_',iteration,sep='')
  if (!column_exists(target_column)) {
    dta <- av_state$data[[av_state$subset]][[cname]]
    std <- sd(dta)
    mu <- mean(dta)
    std_factor <- NULL
    if (iteration == 1) {
      # remove outliers beyond 3xstd
      std_factor <- 3
    } else if (iteration == 2) {
      # remove outliers beyond 2.5x std
      std_factor <- 2.5
    } else {
      # remove outliers beyond 2 std
      std_factor <- 2
    }
    cat("Removing outliers outside of ",std_factor,
        "x std. min: ",mu-std_factor*std,", max: ",
        mu+std_factor*std,"\n",sep='')
    av_state$data[[av_state$subset]][[target_column]] <<- 
      ((dta <= mu-std_factor*std) | (dta > mu+std_factor*std))+0
  }
  target_column
}
