# global print and test functions

print.av_state <- function(x,...) {
  nn <- names(x)
  ll <- length(x)
  cat("\n")
  for (i in seq_len(ll)) {
    cat(nn[i],": ",sep="")
    if (nn[i] == 'raw_data') {
      cat(paste(dim(x[[i]])[1],'samples with',dim(x[[i]])[2],'features'),
          " [missing: ",calc_missing(x[[i]]),"]\n",sep="")
    } else if (nn[i] == 'data') {
      if (class(x[[i]]) == 'list') {
        cat("\n")
        for (j in names(x[[i]])) {
         cat("  ",j,": ",paste(dim(x[[i]][[j]])[1],'samples with',dim(x[[i]][[j]])[2],'features'),
             " [missing: ",calc_missing(x[[i]][[j]]),"]\n",sep="")
        }
      } else {
        cat(paste(dim(x[[i]])[1],'samples with',dim(x[[i]])[2],'features'),"\n")
      }
    } else if (nn[i] %in% c('model_queue',
                            'accepted_models',
                            'rejected_models',
                            'resids',
                            'log_resids')) {
      cat("list with",length(x[[i]]),"models\n")
    } else {
      if (class(x[[i]]) == 'var_model') {
        print(x[[i]],x)
      } else if (class(x[[i]]) == 'list') {
        print(x[[i]])
      } else {
        cat(x[[i]],"\n")
      }
    }
  }
  cat("\n")
  invisible(x)
}

exogvars_to_string <- function(av_state,x,model) {
  str <- "\n    "
  for (i in 1:nr_rows(x)) {
    if (i > nr_rows(x)) { break }
    exovar <- model$exogenous_variables[i,]
    if (i != 1) {
      str <- paste(str,"\n    ",sep='')
    }
    iteration <- exovar$iteration
    if (!is.null(av_state) && av_state$simple_models) iteration <- 3
    str <- paste(str,
                 std_factor_for_iteration(iteration),
                 "x std of res. ",
                 prefix_ln_cond(exovar$variable,model),sep='')
    outliers <- '???'
    if (!is.null(av_state)) {
      outliers <- get_outliers_as_string(av_state,exovar$variable,exovar$iteration,model)
    }
    if (outliers == '') {
      str <- paste(str,' (empty)',sep='')
    } else {
      str <- paste(str,' (obs.: ',outliers,')',sep='')
    }
  }
  str <- paste(str,"",sep='')
  str
}

var_model_to_string <- function(av_state,x,skip_restrict=FALSE) {
  str <- "\n  "
  nn <- names(x)
  ll <- length(x)
  nns <- seq_len(ll)
  if (!is.null(names(x))) {
    nns <- sort(names(x),index.return=TRUE)
    nns <- nns$ix
  }
  for (i in nns) {
    if (skip_restrict == TRUE && nn[i] == "restrict") { next }
    if (str != '\n  ') {
      str <- paste(str,"\n  ",sep='')
    }
    if (class(x[[i]]) == "data.frame") {
      str <- paste(str,nn[i],": ",exogvars_to_string(av_state,x[[i]],x),sep='')
    } else {
      str <- paste(str,nn[i],": ",x[[i]],sep='')
    }
  }
  str
}
print.var_model <- function(x,av_state=NULL,...) {
  cat(var_model_to_string(av_state,x),"\n")
  invisible(x)
}

print.var_modelres <- function(x,av_state=NULL,...) {
  str <- paste(printed_model_score(x$varest)," : ",vargranger_line(x$varest),
               var_model_to_string(av_state,x$parameters,skip_restrict=!is.null(av_state)),
               restrictions_toline(x$varest),sep='')
  cat(str,"\n")
  invisible(x)
}

restrictions_toline <- function(varest) {
  r <- ''
  if (!is.null(varest$restrictions)) {
    r <- paste('\n  constraints: ',restrictions_tostring(varest,format_output_like_stata=FALSE),sep='')
  }
  r
}

print.model_list <- function(x,av_state=NULL,...) {
  i <- 0
  for (model in x) {
    i <- i+1
    cat("[[",i,"]]\n",sep='')
    print(model,av_state)
  }
  cat("\n")
}

new_av_state <- function() {
  x <- list()
  class(x) <- 'av_state'
  x
}

assert_av_state <- function(av_state) {
  if (is.null(av_state) || class(av_state) != 'av_state') {
    stop("invalid av_state argument")
  }
}

load_test_data <- function() {
  av_state <- new_av_state()
  av_state$file_name <- 'data_test.sav'
  av_state$real_file_name <- 'data_test.sav'
  av_state$file_type <- 'SPSS'
  av_state$raw_data <- generate_test_data()
  av_state$data <- list('multiple'=av_state$raw_data)
  av_state
}

generate_test_data <- function() {
  data.frame(id=rep(1,times=5),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'))
}
generate_numerical_test_data <- function(n) {
  ids <- rep(1,times=n)
  somevals <- NULL
  somethings <- NULL
  for (i in 1:n) {
    somevals <- c(somevals,ifelse(runif(1) < 0.25,NA,runif(1,1,n)))
    somethings <- c(somethings,ifelse(runif(1) > 0.8,NA,runif(1,1,n)*runif(1,1,n)))
  }
  data.frame(id=ids,someval=somevals,something=somethings)
}
