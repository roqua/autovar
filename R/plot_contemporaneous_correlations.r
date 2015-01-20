contemporaneous_correlations_graph <- function(av_state) {
  count <- list()
  value <- list()
  vrs <- av_state$vars
  if (is.null(vrs) || length(vrs) < 2 || 
      is.null(av_state$accepted_models) ||
      length(av_state$accepted_models) == 0) return(NULL)
  n <- length(vrs)
  for (i in 1:n)
    for (j in 1:n) {
      if (i == j) next
      # identify insignificant relationships by double spaces
      t <- paste(sort(c(vrs[[i]],vrs[[j]])),collapse="  ")
      count[[t]] <- 0
      count[[paste(vrs[[i]],vrs[[j]])]] <- 0
      value[[paste(vrs[[i]],vrs[[j]])]] <- 0
    }
  r <- list()
  r$allcount <- 0
  r$nonecount <- 0
  for (model in av_state$accepted_models) {
    signmat <- significance_matrix(summary(model$varest))
    r$allcount <- r$allcount+1
    foundsomething <- FALSE
    for (i in 1:(n-1))
      for (j in (i+1):n) {
        if (signmat[j*2,i] > av_state_significance(model$varest) || signmat[j*2-1,i] == 0) {
          t <- paste(sort(c(vrs[[i]],vrs[[j]])),collapse="  ")
          count[[t]] <- count[[t]]+1
        } else {
          foundsomething <- TRUE
          t <- paste(sort(c(vrs[[i]],vrs[[j]]),decreasing = (signmat[j*2-1,i] < 0)),collapse=" ")
          count[[t]] <- count[[t]]+1
          value[[t]] <- value[[t]]+signmat[j*2-1,i]
        }
      }
    if (!foundsomething) r$nonecount <- r$nonecount + 1
  }
  r$str <- ""
  r$edgelabels <- NULL
  r$edgecolors <- NULL
  r$curved <- NULL
  emptyresults <- TRUE
  for (i in 1:n)
    for (j in 1:n) {
      if (i == j) next
      posrel <- (vrs[[i]] < vrs[[j]])
      if (posrel) {
        t <- paste(vrs[[i]],vrs[[j]],sep="  ")
        if (count[[t]] > 0) {
          emptyresults <- FALSE
          r$str <- paste(r$str,t," ",2*count[[t]],"\n",sep="")
          r$curved <- c(r$curved,FALSE)
          r$edgelabels <- c(r$edgelabels,paste(count[[t]]," model",ifelse(count[[t]] == 1,"","s"),
                                               "\n(no sign. correlation)",sep=""))
          r$edgecolors <- c(r$edgecolors,color_for_sign(" "))
        }
      }
      t <- paste(vrs[[i]],vrs[[j]])
      if (count[[t]] == 0) next
      emptyresults <- FALSE
      value[[t]] <- value[[t]] / count[[t]]
      r$str <- paste(r$str,t," ",2*count[[t]],"\n",sep="")
      r$curved <- c(r$curved,TRUE)
      r$edgelabels <- c(r$edgelabels,paste(count[[t]]," model",ifelse(count[[t]] == 1,"","s"),
                                           "\n",ifelse(posrel,"+",""),round(value[[t]],digits=3),sep=""))
      r$edgecolors <- c(r$edgecolors,color_for_sign(ifelse(posrel,"+","-")))
    }
  if (emptyresults) return(NULL)
  r
}
# TODO: textual contemporaneous correlation summary

# TODO: refactoring, lots of double code with vargranger_plot
# TODO: can easily use one function for both graphs
contemporaneous_correlations_plot <- function(av_state) {
  graphi <- contemporaneous_correlations_graph(av_state)
  if (!is.null(graphi)) {
    graphstring <- graphi$str
    # TODO: check if temp files are cleaned up
    file <- tempfile()
    #cat("tempfile:",file,"\n")
    cat(graphstring, file = file)
    a <- read.graph(file,format="ncol",directed=TRUE,weights="yes")
    cols <- c('springgreen4','steelblue','chocolate1','violet','indianred1','lightgoldenrod1')
    E(a)$width <- E(a)$weight
    V(a)$label <- sapply(V(a)$name,function(x) {
      if (!is.null(get_var_label(av_state,x)) && get_var_label(av_state,x) != "") {
        paste(strwrap(paste(get_var_label(av_state,x)," [",x,"]",sep=''),width=15),
              collapse="\n")
      } else {
        x
      }
    })
    E(a)$curved <- graphi$curved
    E(a)$label <- graphi$edgelabels
    E(a)$color <- graphi$edgecolors
    plot(a,
         incenter=!graphi$curved,
         edge.arrow.size=2,
         edge.arrow.width=2,
         edge.arrow.mode=0,
         edge.label.family='sans',
         edge.label.color=colors()[[190]],
         edge.label.cex=0.75,
         vertex.size=65,
         vertex.label.family='sans',
         vertex.label.cex=1,
         vertex.color=cols[order(V(a)$name)],
         vertex.label.color='black',
         vertex.label.font=1,
         main="Contemporaneous correlations",
         sub=paste('found significant contemporaneous correlations in',graphi$allcount - graphi$nonecount,'out of',graphi$allcount,'valid models'))
    igraph_legend_concor()
    gname <- gsub("\\.[^ ]{3,4}$","",basename(av_state$real_file_name))
    gname <- paste(gname,"_concor",sep="")
    fname <- gname
    i <- 0
    while (file.exists(paste(fname,'.pdf',sep=''))) {
      i <- i + 1
      fname <- paste(gname,'_',i,sep='')
    }
    fname1 <- paste(fname,'.pdf',sep='')
    i <- i + 1
    fname <- paste(gname,'_',i,sep='')
    while (file.exists(paste(fname,'.pdf',sep=''))) {
      i <- i + 1
      fname <- paste(gname,'_',i,sep='')
    }
    fname2 <- paste(fname,'.pdf',sep='')
    dev.copy2pdf(file=fname1)
    fsize1 <- file.info(fname1)$size
    dev.copy2pdf(file=fname2)
    fsize2 <- file.info(fname2)$size
    if (fsize1 != fsize2) {
      #warning("file sizes not equal")
    }
    if (fsize2 > fsize1) {
      file.remove(fname1)
      fname <- fname2
    } else {
      file.remove(fname2)
      fname <- fname1
    }
    if (interactive() && !exists("currently_generating_help_files")) {
      scat(av_state$log_level,3,
           "\nContemporaneous correlations plot saved to \"",
           fname,"\" (",file.info(fname)$size,")\n",sep='')
    }
    invisible(a)
  }
}
igraph_legend_concor <- function() {
  cols <- colors()[c(517,33,345)]
  str <- c('positive correlation',
           'negative correlation',
           'no significant correlation')
  mtext(str,side=1,line=0:2,col=cols,font=2,adj=0,cex=0.8)
}
