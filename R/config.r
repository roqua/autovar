# config

# load default values
.onLoad <- function(libname,pkgname) {
  reset_state()
}

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
    } else {
      cat(x[[i]],"\n")
    }
  }
  cat("\n")
  invisible(x)
}

reset_state <- function() {
  x <- list()
  class(x) <- 'av_state'
  assign('av_state',x,pos='.GlobalEnv')
}

print_state <- function() {
  print(av_state)
}

load_test_data <- function() {
  reset_state()
  av_state$file_name <<- 'data_test.sav'
  av_state$real_file_name <<- 'data_test.sav'
  av_state$file_type <<- 'SPSS'
  av_state$raw_data <<- generate_test_data()
  av_state$data <<- list('1'=av_state$raw_data)
}

generate_test_data <- function() {
  data.frame(id=rep(1,times=5),tijdstip=c(1,3,5,6,7),home=c('yes','no','yes',NA,'yes'))
}
