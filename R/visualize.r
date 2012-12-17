# visualize

visualize <- function(columns,...) {
  if (length(columns) == 1) {
    visualize_column(columns,...)
  } else {
    visualize_columns(columns,...)
  }
}

visualize_column <- function(column,...) {
  if (class(av_state$data[[1]][[column]]) == "factor") {
    visualize_categorical_column(column,...)
  } else if (class(av_state$data[[1]][[column]]) == "numeric") {
    visualize_scale_column(column,...)
  } else {
    stop(paste("unknown column class",class(av_state$data[[1]][[column]]),
     "for column",column))
  }
}

visualize_scale_column <- function(column,type=c('LINE','BOX'),title="",...) {
  visualize_method <- match.arg(type)
  visualize_method <- switch(visualize_method,
    LINE = visualize_line,
    BOX = visualize_box
  )
  idx <- 0
  n<-length(av_state$data)
  x<-floor(sqrt(n))
  y<-ceiling(n/x)
  op <- par(mfrow=c(x,y))
  for (data_frame in av_state$data) {
    idx <- idx+1
    data_frame[[column]][is.na(data_frame[[column]])] <- 0
    visualize_method(column,data_frame[[column]],paste(title,visualize_sub_title(idx),sep=''),...)
  }
  par(op)
}

visualize_categorical_column <- function(column,type=c('PIE','BAR','DOT'),title="",...) {
  visualize_method <- match.arg(type)
  visualize_method <- switch(visualize_method,
    PIE = visualize_pie,
    BAR = visualize_bar,
    DOT = visualize_dot
  )
  idx <- 0
  n<-length(av_state$data)
  x<-floor(sqrt(n))
  y<-ceiling(n/x)
  old.par <- par(no.readonly = TRUE)
  par(oma = c( 0, 0, 3, 0 ),mfrow=c(x,y))
  for (data_frame in av_state$data) {
    clevels <- levels(data_frame[[column]])
    used_levels <- NULL
    idx <- idx+1
    slices <- NULL
    colors <- NULL
    ccolors <- rainbow(length(clevels)+1)
    i <- 1
    ccolor <- ccolors[[i]]
    totalcolumn <- length(which(is.na(data_frame[[column]])))
    if (totalcolumn > 0) {
      slices <- c(slices,totalcolumn)
      used_levels <- c(used_levels,'NA')
      colors <- c(colors,ccolor)
    }
    for (clevel in clevels) {
      i <- i+1
      ccolor <- ccolors[[i]]
      totalcolumn <- length(which(data_frame[[column]] == clevel))
      if (totalcolumn > 0) {
        slices <- c(slices,totalcolumn)
        used_levels <- c(used_levels,clevel)
        colors <- c(colors,ccolor)
      }
    }
    visualize_method(slices,labels=used_levels,main=paste(title,visualize_sub_title(idx),sep=''),colors=colors,...)
  }
  mtext(column,outer = TRUE)
  par(old.par)
}

visualize_columns <- function(columns,labels=columns,type=c('PIE','BAR','DOT'),title="",...) {
  visualize_method <- match.arg(type)
  visualize_method <- switch(visualize_method,
    PIE = visualize_pie,
    BAR = visualize_bar,
    DOT = visualize_dot
  )
  if (length(columns) != length(labels)) {
    stop("Length of columns is not the same as length of labels")
  }
  idx <- 0
  n<-length(av_state$data)
  x<-floor(sqrt(n))
  y<-ceiling(n/x)
  old.par <- par(no.readonly = TRUE)
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
      if (class(data_frame[[column]]) != "numeric") {
        warning(paste("cannot plot nonnumeric column",column))
      } else {
        totalcolumn <- sum(data_frame[[column]], na.rm=TRUE)
        if (totalcolumn > 0) {
          slices <- c(slices,totalcolumn)
          used_columns <- c(used_columns,clabel)
          colors <- c(colors,ccolor)
        }
      }
    }
    visualize_method(slices,labels=used_columns,main=paste(title,visualize_sub_title(idx),sep=''),colors=colors,...)
  }
  par(old.par)
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

visualize_dot <- function(columns,labels,main,colors,...) {
  dotchart(columns,main=main,col=colors,labels=labels,...)
}

visualize_bar <- function(columns,labels,main,colors,...) {
  barplot(columns,main=main,col=colors,names.arg=labels,...)
}

visualize_line <- function(column,y,main,acc=FALSE,...) {
  if (acc) {
    i <- 0
    for (yv in y) {
      i <- i+1
      if (i > 1) {
        y[i] <- yv + y[i-1]
      }
    }
  }
  xla <- 'index'
  if (!is.null(av_state$order_by)) {
    xla <- paste(av_state$order_by,'index')
  }
  plot(1:length(y),y=y,type='p',main=main,
   ylab=column,xlab=xla,pch=18,...)
  lines(1:length(y),y=y,type='l',...)
  #coords <- data.frame(cbind(1:length(y),y))
  #names(coords) <- c("x", "y")
  #lines(as.data.frame(with(coords, list(x = spline(x)$y, y = spline(y)$y))),...)
}

visualize_box <- function(column,y,main,...) {
  boxplot(y,main=main,ylab=column,...)
}
