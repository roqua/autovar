# pie_chart

pie_chart <- function(columns,labels=columns,title="") {
  if (length(columns) != length(labels)) {
    error("Length of columns is not the same as length of labels")
  }
  idx <- 0
  used_columns <- NULL
  n<-length(av_state$data)
  x<-floor(sqrt(n))
  y<-ceiling(n/x)
  par(mfrow=c(x,y))
  for (data_frame in av_state$data) {
    idx <- idx+1
    slices <- NULL
    i <- 0
    for (column in columns) {
      i <- i+1
      clabel <- labels[[i]]
      totalcolumn <- sum(data_frame[[column]], na.rm=TRUE)
      if (totalcolumn > 0) {
        slices <- c(slices,totalcolumn)
        used_columns <- c(used_columns,clabel)
      }
    }
    #print(slices)
    pie(slices,labels=used_columns,main=paste(title,pie_sub_title(idx),sep=''))
  }
}

pie_sub_title <- function(idx) {
  if (is.null(av_state[['group_by']])) {
    ""
  } else {
    id_field <- av_state[['group_by']]
    paste(' ',id_field,' = ',av_state$data[[idx]][[id_field]][1],sep='')
  }
}
