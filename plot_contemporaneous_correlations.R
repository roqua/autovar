igraph_legend1 <- function() {
  cols <- colors()[c(517,123,33)]
  str <- c('positive associations','negative associations')
  mtext(str,side=1,line= 1:2,col=cols,font=2,adj=0,cex=0.8)
}
plot_contemporaneous_correlations <- function(av_state) {
  a <- summary(av_state$accepted_models[[1]]$varest)
  p <- NULL
  n <- length(av_state$vars)
  r <- av_state$vars[[(n)]]
  for (i in 1:n) {
    p <- c(p,av_state$vars[i])
  }
  s <- NULL
  q <- NULL
  for(i in 1:(n-1)) {
    if ( i > (n-1)) { break } 
    s <-c(s,av_state$vars[i])
    q[i]<-i
  }
  s<-c(r,s)
  q<-c(n,q)
  z<- data.frame(name=c(p)) 
  relations <- data.frame(from=c(p),
                          to=c(s))
  g <- graph.data.frame(relations, directed=FALSE, vertices=z)
  for(i in 1:n) {
    if (i > n) { break }
    E(g)$weight[[i]] <- a$corres[i,(q[i])]
  }
  sign <- NULL
  for(i in 1:n) {
    if ( i > n) { break }
    if (E(g)$weight[[i]]>0) { sign[i] <- 'palegreen'}
    else if (E(g)$weight[[i]]<0)  { sign[i] <- 'brown1'}
    else if (E(g)$weight[[i]]==0) { sign[i] <- 'deepskyblue2'}
    else { sign[i] <- 'gray15'}
  }
cols <- c('springgreen4','steelblue','chocolate1')
E(g)$width <- E(g)$weight
E(g)$width <- 20*abs(E(g)$width)*1/max(abs(E(g)$width))
E(g)$color <- sign
E(g)$label <- round(E(g)$weight,2)
plot(g,layout=layout.kamada.kawai, 
     edge.arrow.size=2,
     edge.arrow.width=2,
     edge.label.family='sans',
     edge.label.color='black',
     edge.label.cex=1.25,
     vertex.size=65,
     vertex.label.cex=1,
     vertex.color=cols[1:(length(V(g)))],
     vertex.label.color='black',
     vertex.label.font=1,
     edge.curved=FALSE,
     main="Contemporaneous Correlations")
  igraph_legend1()
  invisible(g)
}
