#'this function allows to center and reduce the matrix or the numerical vector X of the predictive variables if \code {reduce = T}.
#' @usage
#' plsda.scale(X,reduce = F)
#' @param
#' X The matrix that must be centered and standardized if \code{reduce = T}.
#' @param
#' reduce is an optional parameters which if \code{TRUE} standardizes the data frame or numeric vector X.
#' @return
#' The function returns a list of at least the following components :
#' @return
#' \code{New} is the resulting centered (and standardized if \code{reduce = T}) matrix.
#' \cr
#' \code{means} is the average of the initial matrix or numeric vector X.
#' \cr
#' \code{vars} is the variance of the initial matrix or numeric vector X.
#' @examples
#'scale.t1<-plsda.scale(iris[,-5])
#'scale.t2<-plsda.scale(iris[,-5],reduce=T)

plsda.scale <- function(X,reduce = FALSE){
  #vérifier si la matrice est numeric
  X<-as.matrix(X)
  if(typeof(X)!="double"){
    stop("Not expected character's matrix")
  }
  #création de l'instance qui retourne matrice centrée réduite , la moyenne de chaque vble et son écart type sous forme de liste
  instance <- list()
  #application de la fonction mean à chaque colonne du matrice X
  instance$mean <- apply(X,2,mean)
  instance$new <- X-instance$mean
  #condition sur le paramètre reduce
  if(reduce){
    #application de la fonction sd à chaque colonnes du matrice X
    instance$sd <- apply(X,2,sd)
    #centrage et réduction de toutes les lignes du matrice x
    instance$new_X <- apply(X,1,function(x){ return((x-instance$mean)/instance$sd)})
    class(instance) <- "scale"
    return(instance)
  }else{
    return(list("New"=as.matrix(instance$new),
                "means"=instance$mean))
  }
}

data(iris)
print(iris)
object <- plsda.scale(iris[,1:4],reduce=TRUE)
print(object)

#surcharger la méthode print

print.scale <- function(objet){
  cat("Moyenne","\n")
  mean <- objet$mean
  print(mean)
  cat("Standard deviation","\n")
  sd <- objet$sd
  print(sd)
  cat("New","\n")
  new<- as.matrix(objet$new_X)
  print(new)

}
print(object)

