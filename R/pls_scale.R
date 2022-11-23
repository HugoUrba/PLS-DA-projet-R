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
  #Check wether the matrix is numeric
  X<-as.matrix(X)
  if(typeof(X)!="double"){
    stop("Not expected character's matrix")
  }
  #creation of the instance which returns reduced-centered matrix. Each variable-mean and its standard deviation are collected in a list
  instance <- list()
  #function mean applied row per row to each X column
  instance$mean <-apply(X,1,function(x){return(x-mean(x))})
  #condition on reduce
  if(reduce==TRUE){
    #compute the coefficients of the matrix in dividing the standard deviation
    
    instance$new_X <- apply(instance$mean,1,function(x){return(x/sd(x))})
    instance$new_mean_col <- apply(instance$new_X,2,mean)
    instance$new_mean_sd <- apply(instance$new_X,2,sd)
    class(instance) <- "scale"
    return(instance)
  
  }
}

data(iris)
print(iris)
object <- plsda.scale(iris[,1:4],reduce=TRUE)
print(object)

#surcharger la méthode print

print.scale <- function(objet){
  cat("New column mean (should be around 0)",":")
  new_mean<-as.matrix(objet$new_mean_col)
  print(new_mean)
  cat("New column standard deviation (must be 1)",":")
  new_st<-as.matrix(objet$new_mean_sd)
  print(new_st)
  cat("New values after scaling processing ",":")
  new_x <- objet$new_X
  print(head(new_x))

}
print(object)

