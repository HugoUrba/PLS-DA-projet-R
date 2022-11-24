# transform a Factor to Dummy Matrix
#'
#' This function transforms a vector of \code{p} factors into a matrix of \code{p} dummies variables.
#' @usage
#' plsda.dummies(X, name)
#' @param
#' X the vector of factors to transform.
#' @param
#' name a vector containing the original variables names that will be the \code{colnames} of the dummy matrix.
#' @return
#' The function returns a dummy matrix with \code{p} columns, named with the vector "\code{name}".
#' @examples
#' dummies.t1<-plsda.dummies(iris$Species)
#' dummies.t2<-plsda.dummies(iris$Species,"Species")
#'@export


# X = Vecteur (de facteurs) à transformer
# name = Nom de la varaiable initale
plsda.dumnies <- function (X,name){
  instance <- list()
  # renvoyer la colonne spécifiée X en tant que facteur plutôt que numérique.
  instance$fX<- as.factor(as.vector(X))
  # les Modalités du facteur
  instance$levels <- levels(instance$fX)
  # Matrice d'indicatrice
  instance$dum <- sapply(instance$levels,function(x){ifelse(instance$fX==x,1,0)})
  class(instance) <- "dumnies"
  return(instance)
}


#surcharger la méthode print pour afficher la matrice des variables indicatrices

print.dumnies <- function(objet){
  cat("dumny species ","\n")
  dumny <- as.matrix(objet$dum)
  print(dumny)

}
