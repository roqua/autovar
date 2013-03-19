#' Visualize columns of the data set
#' 
#' This function works with single or multiple columns. When given an array of multiple columns as \code{columns} argument, all nonnumeric columns are converted to numeric class in the plot. This function creates a combined plot with individual plots for each identified group (if the \code{\link{group_by}} was used) in the current data set. Any supplied arguments other than the ones described are passed on to the plotting functions.
#' @param av_state an object of class \code{av_state}
#' @param columns specifies the columns to be displayed. When given the name of a single column, the function behaves differently depending on the class of the column: \itemize{
#' \item If the class of the column is \code{factor}, the column is seen as a nominal column, and the following arguments are accepted: \code{visualize(column,type=c('PIE','BAR','DOT','LINE'),title="",...)}. All plots also accept the \code{xlab} argument, e.g., \code{xlab='minuten'}. Furthermore, when the type is \code{'BAR'}, an additional argument \code{horiz} can be supplied (\code{horiz} is \code{FALSE} by default), which will draw horizontal bar charts instead of vertical ones. To show values over time rather than total values, the \code{'LINE'} type can be used. Example: \code{visualize('PHQ1')} (assuming \code{'PHQ1'} is a \code{factor} column).
#' \item If the class of the column is \code{numeric}, the column is seen as a scale column, and the following arguments are accepted: \code{visualize(column,type=c('LINE','BOX'),title="",...)}. Furthermore, when the type is \code{'LINE'}, an additional argument \code{acc} can be supplied (\code{acc} is \code{FALSE} by default), which will plot lines of accumulated values rather than the individual values. Example: \code{visualize('minuten_sport',type='LINE',acc=TRUE)} (assuming \code{'minuten_sport'} is a \code{numeric} column).
#' }
#' When the \code{columns} argument is given a vector of column names, the columns are either shown as multiple lines in a line plot (when \code{type='LINE'}), or the sums of the columns are displayed in the plots (for any of the other types). When given a vector of column names as the \code{columns} argument, the function accepts the following arguments: 
#' \code{visualize(columns,labels=columns,type=c('LINE','PIE','BAR','DOT'),
#'                 title="",...)}.
#' The arguments of this function work much like the ones described above for individual \code{factor} columns. The added optional \code{labels} argument should be a vector of the same length as the \code{columns} argument, specifying custom names for the columns. This argument is ignored when \code{type='LINE'}.
#' @examples
#' av_state <- load_file("../data/input/RuwedataAngela.sav")
#' av_state <- add_derived_column(av_state,'sum_minuten_licht',
#'               c('minuten_woonwerk','minuten_werk_licht',
#'                 'minuten_licht_huishouden'))
#' av_state <- add_derived_column(av_state,'sum_minuten_zwaar',
#'               c('minuten_zwaarhuishouden','minuten_zwaar_werk'))
#' visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar',
#'           'minuten_vrijetijd','minuten_sport'),
#'           labels=c('licht werk','zwaar werk','vrije tijd','sport'),
#'           type='BAR',horiz=TRUE)
#' visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar',
#'           'minuten_vrijetijd','minuten_sport'),
#'           type='DOT',xlab='minuten')
#' visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar',
#'           'minuten_vrijetijd','minuten_sport'))
#' @export
visualize <- function(av_state,columns,...) {
  assert_av_state(av_state)
  if (length(columns) == 1) {
    visualize_column(av_state,columns,...)
  } else {
    visualize_columns(av_state,columns,...)
  }
}

visualize_column <- function(av_state,column,...) {
  if (class(av_state$data[[1]][[column]]) == "factor") {
    visualize_nominal_column(av_state,column,...)
  } else if (class(av_state$data[[1]][[column]]) == "numeric") {
    visualize_scale_column(av_state,column,...)
  } else {
    stop(paste("unknown column class",class(av_state$data[[1]][[column]]),
     "for column",column))
  }
}

visualize_scale_column <- function(av_state,column,type=c('LINE','BOX'),title="",...) {
  visualize_methodr <- match.arg(type)
  visualize_method <- switch(visualize_methodr,
    LINE = visualize_line,
    BOX = visualize_box
  )
  idx <- 0
  n<-length(av_state$data)
  x<-floor(sqrt(n))
  y<-ceiling(n/x)
  if (visualize_methodr == 'BOX') {
    op <- par(mfrow=c(x,y))
  } else {
    op <- par(oma=c(0,3,2,0),mfrow=c(x,y))
  }
  for (data_frame in av_state$data) {
    idx <- idx+1
    if (class(data_frame[[column]]) != "factor") {
      data_frame[[column]][is.na(data_frame[[column]])] <- 0
    }
    visualize_method(av_state$order_by,column,data_frame[[column]],
                     paste(title,
                           visualize_sub_title(av_state[['group_by']],
                                               av_state$data[[idx]]),sep=''),
                     ...)
  }
  par(op)
}

visualize_nominal_column <- function(av_state,column,type=c('PIE','BAR','DOT','LINE'),title="",...) {
  visualize_method <- match.arg(type)
  if (visualize_method == 'LINE') {
    visualize_scale_column(av_state,column,type=visualize_method)
  } else {
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
      visualize_method(slices,labels=used_levels,
                       main=paste(title,
                                  visualize_sub_title(av_state[['group_by']],
                                                      av_state$data[[idx]]),sep=''),
                       colors=colors,...)
    }
    mtext(column,outer = TRUE)
    par(old.par)
  }
}

visualize_columns <- function(av_state,columns,labels=columns,type=c('LINE','PIE','BAR','DOT'),title="",...) {
  visualize_method <- match.arg(type)
  if (visualize_method == 'LINE') {
    visualize_lines(av_state,columns,labels,title,...)
  } else {
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
          cat(paste("plotting nonnumeric column as numeric, converting...",column),"\n")
          data_frame[[column]] <- as.numeric(data_frame[[column]])
        }
        totalcolumn <- sum(data_frame[[column]], na.rm=TRUE)
        if (totalcolumn > 0) {
          slices <- c(slices,totalcolumn)
          used_columns <- c(used_columns,clabel)
          colors <- c(colors,ccolor)
        }
      }
      visualize_method(slices,labels=used_columns,
                       main=paste(title,
                                  visualize_sub_title(av_state[['group_by']],
                                                      av_state$data[[idx]]),sep=''),
                       colors=colors,...)
    }
    par(old.par)
  }
}

visualize_lines <- function(av_state,columns,labels,title,...) {
  if (length(columns) != length(labels)) {
    stop("Length of columns is not the same as length of labels")
  }
  idx <- 0
  n<-length(av_state$data)
  x<-floor(sqrt(n))
  plots <- list()
  for (data_frame in av_state$data) {
    idx <- idx+1
    cdata <- NULL
    idvar <- NULL
    for (column in columns) {
      if (class(data_frame[[column]]) != 'numeric') {
        data_frame[[column]] <- as.numeric(data_frame[[column]])
      }
    }
    if (!is.null(av_state$order_by)) {
      cdata <- data_frame[c(columns,av_state$order_by)]
      idvar <- av_state$order_by
    } else {
      idvar <- 'index'
      while (any(idvar == names(data_frame))) { 
        id_var <- paste(idvar,'_',sep='')
      }
      dnames <- names(data_frame[columns])
      cdata <- cbind(data_frame[columns],1:(dim(data_frame[columns])[[1]]))
      names(cdata) <- c(dnames,idvar)
    }
    mdata <- melt(cdata,id.vars = idvar)
    plots[[idx]] <- ggplot(mdata, aes_string(x = idvar, y = 'value', colour = 'variable')) + 
            geom_line() + 
            geom_point() +
            ggtitle(paste(title,visualize_sub_title(av_state[['group_by']], 
                                                    av_state$data[[idx]]),sep=''))
  }
  plots[['ncol']] <- 1 # x
  do.call(grid.arrange,plots)
}

visualize_sub_title <- function(group_by_field,data_subset) {
  if (is.null(group_by_field)) {
    ""
  } else {
    id_field <- group_by_field
    paste(' ',id_field,' = ',data_subset[[id_field]][1],sep='')
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

visualize_line <- function(order_by_field,column,y,main,acc=FALSE,...) {
  yorig <- y
  y <- as.numeric(y)
  y[is.na(y)] <- 0
  if (acc) {
    i <- 0
    for (yv in y) {
      i <- i+1
      if (i > 1) {
        y[i] <- yv + y[i-1]
      }
    }
  }
  mat <- sort(as.numeric(unique(y)))
  mlabels <- as.character(signif(mat,digits=2))
  if (class(yorig) == 'factor' && !acc) {
    mat <- 0:length(levels(yorig))
    mlabels <- c('NA',levels(yorig))
  }
  xla <- 'index'
  if (!is.null(order_by_field)) {
    xla <- paste(order_by_field,'index')
  }
  if (acc || length(mat) > 10) {
    plot(1:length(y),y=y,type='p',main=main,
         ylab='',xlab=xla,pch=18,...)
  } else {
    plot(1:length(y),y=y,type='p',main=main,
         ylab='',xlab=xla,pch=18,yaxt="n",...)
    axis(2,at=mat,labels=mlabels, las=2)
  }
  lines(1:length(y),y=y,type='l',...)
  title(main=column,outer=TRUE)
}

visualize_box <- function(order_by_field,column,y,main,...) {
  boxplot(y,main=main,ylab=column,...)
}
