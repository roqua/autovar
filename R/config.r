# config

# load default values
.onLoad <- function(libname,pkgname) {
  x <- list()
  class(x) <- 'av_state'
  assign('av_state',x,pos='.GlobalEnv')
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

print_state <- function() {
  print(av_state)
}
