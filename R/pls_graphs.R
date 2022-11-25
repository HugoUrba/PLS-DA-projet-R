#'@export
plsda.scree <- function(objetPLSDA){
  X <- objetPLSDA$X
  corrX <- cor(X)
  ev <- eigen(corrX)
  ev1 = ev$values
  scree <- plot(ev1,
                type="b",
                ylab="Eigen values",
                xlab="Components", 
                main ="Scree plot")
}

#'@export
plsda.vmap <- function(objetPLSDA){
  X <- objetPLSDA$X
  corrX <- cor(X)
  ev <- eigen(corrX)$values
  et <- sqrt(ev)[1:2]
  lx <- objetPLSDA$Xloadings[,1:2]
  c <- t(apply(lx,1,function(x) x*et))

  plot(c[,1], c[,2], 
       xlim=c(-1,+1), 
       ylim=c(-1,+1), 
       type = "n",
       asp = 1,
       ylab="Component 2",
       xlab="Component 1", 
       main ="Variables on the 2 first components")
  abline(h=0,v=0)
  text(c[,1], c[,2],labels=rownames(c))
  symbols(0, 0, circles=1, inches=F, add=T)
}

#'@export
plsda.Imap <- function(objetPLSDA){
  
  library(RColorBrewer)
  color <- apply(objetPLSDA$Y, 1, which.max)
  color <- brewer.pal(length(objetPLSDA$modalities), "Set1")[color]
  plot(objetPLSDA$Xscores[,1],
       objetPLSDA$Xscores[,2],
       type = "n",
       ylab="Component 2",
       xlab="Component 1", 
       main ="Indivuals on the 2 first components per modality")
  abline(h=0,v=0)
  text(objetPLSDA$Xscores[,1],objetPLSDA$Xscores[,2],labels=rownames(objetPLSDA$Xscores),cex=0.75,col = color)
}
