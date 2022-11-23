# #This function randomly separate a data set into a learning sample and test sample that can be selected by the user.
#' @usage
#' plsda.split_sample(data,prop.train=0.75)
#'  @param
#' the dataset that we have to split
#'@param
#' the proportion of the training data
#'@return
#'\code {train} a subset matrice of data corresponding to the training sample dataset.
#'\cr
#'\code{test} a subset matrice of data corresponding to the test sample dataset.
#'@examples

#creation de la classe S3 split
plsda.split_sample<-function(data,prop.train){

  # controle de prop.train
  if(prop.train>1 | prop.train<0){
    stop("Proportion non comprise entre 0 et 1")
  }
  #ok - on peut y aller
  n <- nrow(data)
  #make this example reproducible
  set.seed(1)
  # Selection des indices des individus de l'échantillon d'apprentisage
  index <- sample(1:n, size = n*prop.train ,replace = FALSE)
  #création de l'instance
  instance <- list()
  #echantillon d'apprentissage
  instance$train <- data[index,]
  #taille de l'échantillon d'apprentissage
  instance$train_size <- nrow( instance$train)
  #echantillon de test
  instance$test<- data[-index,]
  #taille de l'échantillon test
  instance$test_size <- nrow(instance$test)
  class(instance) <- "split"
  return(instance)

}


#surcharge de la méthode print de manière à ce qu’elle affiche l'échantillon test et l'échantillon train
print.split <- function(objet){
  #affichage de l'échantillon train
  cat("train","\n")
  train <-  objet$train
  print(train)
  #affichage de l'échantillon test
  cat("test","\n")
  test <- objet$test
  print(test)
}



