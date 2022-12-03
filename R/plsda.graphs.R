#'plsda.graphs displays main help-making-decisions plots
#' Displays the plot scree of the components importance
#'
#' This function displays a screeplot on which we can see the importance of each component by calculating their eigenvalues.
#' @usage
#' plsda.scree(objectPLSDA)
#' @param
#' objectPLSDA, an object returned by the function plsda.fit, the model the user is using.
#' @return
#' The function returns a screeplot, with the components on the abscissa axis and their eigen values on the ordinate axis.

#'@export


#function for the screeplot
plsda.scree <- function(objetPLSDA){
  
  #getting the explanatory variables matrix from the PLSDA object
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

#' Displays the map of the explanatory variables
#'
#' This function displays a screeplot on which we can see the importance of each explanatory variable each explanatory variable by multiplying the square root of their eigenvalues with their loadings (Xloadings).
#' @usage
#' plsda.vmap(objetPLSDA)
#' @param
#' PLSDAObject, an object returned by the function plsda.fit.
#' @return
#' The function returns a screeplot.
#'@export

#function for the variables map
plsda.vmap <- function(objetPLSDA){
  
  #getting the explanatory variables matrix from the objectPLSDA
  X <- objetPLSDA$X
  
  #getting the eigen values of X
  corrX <- cor(X)
  ev <- eigen(corrX)$values
  
  #getting the square root of the eigen values
  et <- sqrt(ev)[1:2]
  
  #getting the loadings of X from the objectPLSDA
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

#' Displays the map of the individuals
#'
#' This function displays a plot.
#' @usage
#' plsda.Imap(objetPLSDA)
#' @param
#' objetPLSDA, an object returned by the function plsda.fit.
#' @return
#' The function returns the map of the individuals.
#'@export

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

#' Displays the map of the explanatory variables
#'
#' @usage
#' plsda.plotx(objetPLSDA)
#' @param
#'objetPLSDA, an object returned by the function plsda.fit.
#' @return
#' The function returns the map of the explanatory variables.
#'@export

#function for the importance of the explanatory variables

#function for the importance of the explanatory variables
plotx <- function(objetPLSDA, x, y){
  
  color <- apply(objetPLSDA$Y, 1, which.max)
  color <- brewer.pal(length(objetPLSDA$modalities), "Set1")[color]
  
  plot(objetPLSDA$X[,x], objetPLSDA$X[,2], type = "p", col=color, pch = 18)
}
