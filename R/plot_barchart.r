
#' Plots a barchart of manual_score and the av_scores
#' 
#' This function plots the bar chart for the AIC and BIC scores of the accepted models and compares the Manual AIC and BIC scores graphically.
#' @param av_state an object of class \code{av_state} that was the result of a call to \code{\link{var_main}}
#' @param manual_scores:   The manual entries that are to be compared with the av_scores
#' @examples
#' av_state <- load_file("../data/input/pp5 nieuw compleet.sav",log_level=3)
#' av_state <- var_main(av_state,c('SomBewegUur','SomPHQ'),criterion="BIC",log_level=3)
#' # av_state is the result of a call to var_main
#' plot_barchart(av_state,20.02,61.48)
#' @export
plot_barchart <- function(av_state, x,y,...) {
  #generate data
  a <-NULL
  b <-NULL
  name <-NULL
  l <- length(av_state$accepted_model)
  for(i in 1:l)
  {
    a <- c(a,estat_ic(av_state$accepted_model[[i]]$varest)$AIC)
    b <- c(b,estat_ic(av_state$accepted_model[[i]]$varest)$BIC)
    name <- c(name,paste(i,'model',sep=''))
  }
  df <- data.frame (Input=c('AIC','BIC'))
  for(i in 1:l)
  {
  df[[name[[i]]]] <- c(a[[i]],b[[i]])
  }
  df[['Manual_model']] <- c(x,y)
  dfm <- melt(df,id.vars = 1)
  ggplot(dfm,aes(x = Input,y = value,fill=variable)) +
  geom_bar(position= "dodge",stat="identity", width=0.5,colour="white")
}
