
#install rcolorBrewer if it is not installed yet
if (!("RColorBrewer" %in% installed.packages())){
  install.packages("RColorBrewer")
}
library("RColorBrewer")

#'@export


#function for the screeplot
plsda.scree <- function(objetPLSDA){
  
  #getting the explicative variables matrix from the PLSDA object
  X <- objetPLSDA$X
  
  #Correlation matrix of X
  corrX <- cor(X)
  
  #getting the eigen values of the matrix
  ev <- eigen(corrX)
  ev1 = ev$values
  
  #making the scree plot
  scree <- plot(ev1,
                type="b",
                ylab="Eigen values",
                xlab="Components", 
                main ="Scree plot")
}

#'@export
#'

#function for the variables map
plsda.vmap <- function(objetPLSDA){
  
  #getting the explicative variables matrix from the PLSDA Object
  X <- objetPLSDA$X
  
  #getting the eigen values of X
  corrX <- cor(X)
  ev <- eigen(corrX)$values
  
  #getting the square root of the eigen values
  et <- sqrt(ev)[1:2]
  
  #getting the loadings of X from the PLSDA Object
  lx <- objetPLSDA$Xloadings[,1:2]
  
  #multiplying the Xloadings with et
  c <- t(apply(lx,1,function(x) x*et))

  #making the circle plot in the first factorial space
  plot(c[,1], c[,2], 
       xlim=c(-1,+1), 
       ylim=c(-1,+1), 
       type = "n",
       asp = 1,
       ylab="Component 2",
       xlab="Component 1", 
       main ="Variables on the 2 first components")
  abline(h=0,v=0)
  
  #naming the axis
  text(c[,1], c[,2],labels=rownames(c))
  
  #forcing the circle to be on the size (-1,1) on the two axis on the display
  symbols(0, 0, circles=1, inches=F, add=T)
}

#'@export
#'

#function for the individuals map
plsda.Imap <- function(objetPLSDA){
  
  color <- apply(objetPLSDA$Y, 1, which.max)
  color <- brewer.pal(length(objetPLSDA$modalities), "Set1")[color]
  
  #creating the plot
  plot(objetPLSDA$Xscores[,1],
       objetPLSDA$Xscores[,2],
       type = "n",
       ylab="Component 2",
       xlab="Component 1", 
       main ="Indivuals on the 2 first components per modality")
  abline(h=0,v=0)
  
  #naming the axis, the size and the colors of the points
  text(objetPLSDA$Xscores[,1],objetPLSDA$Xscores[,2],labels=rownames(objetPLSDA$Xscores),cex=0.75,col = color)
}

#'@export

#function for the importance of the explicative variables
plotx <- function(objetPLSDA, x, y){
  
  color <- apply(objetPLSDA$Y, 1, which.max)
  color <- brewer.pal(length(objetPLSDA$modalities), "Set1")[color]
  
  plot(objetPLSDA$X[,x], objetPLSDA$X[,2], type = "p", col=color, pch = 18)
}
