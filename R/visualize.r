# visualize

visualize <- function(columns,labels=columns,type=c('PIE','BAR','LINE'),title="",...) {
  visualize_method <- match.arg(type)
  visualize_method <- switch(visualize_method,
    PIE = visualize_pie,
    BAR = visualize_bar,
    LINE = visualize_line
  )
  if (length(columns) != length(labels)) {
    error("Length of columns is not the same as length of labels")
  }
  idx <- 0
  n<-length(av_state$data)
  x<-floor(sqrt(n))
  y<-ceiling(n/x)
  par(mfrow=c(x,y))
  for (data_frame in av_state$data) {
    used_columns <- NULL
    idx <- idx+1
    slices <- NULL
    colors <- NULL
    ccolors <- rainbow(length(columns))
    i <- 0
    for (column in columns) {
      i <- i+1
      clabel <- labels[[i]]
      ccolor <- ccolors[[i]]
      totalcolumn <- sum(data_frame[[column]], na.rm=TRUE)
      if (totalcolumn > 0) {
        slices <- c(slices,totalcolumn)
        used_columns <- c(used_columns,clabel)
        colors <- c(colors,ccolor)
      }
    }
    visualize_method(slices,labels=used_columns,main=paste(title,visualize_sub_title(idx),sep=''),colors=colors,...)
  }
}

visualize_sub_title <- function(idx) {
  if (is.null(av_state[['group_by']])) {
    ""
  } else {
    id_field <- av_state[['group_by']]
    paste(' ',id_field,' = ',av_state$data[[idx]][[id_field]][1],sep='')
  }
}

visualize_pie <- function(columns,labels,main,colors,...) {
  pie(columns,labels=labels,main=main,col=colors,...)
}

visualize_line <- function(columns,labels,main,colors,...) {
  cat("visualize_line")
}

visualize_bar <- function(columns,labels,main,colors,...) {
  barplot(columns,main=main,col=colors,names.arg=labels,...)
}

#visualize(c('sum_minuten_licht','sum_minuten_zwaar','minuten_vrijetijd','minuten_sport'),
#          labels=c('licht werk','zwaar werk','vrije tijd','sport'),type='BAR',horiz='TRUE')
#visualize(c('sum_minuten_licht','sum_minuten_zwaar','minuten_vrijetijd','minuten_sport'),
#          labels=c('licht werk','zwaar werk','vrije tijd','sport'),type='BAR')
