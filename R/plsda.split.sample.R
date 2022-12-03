# This function randomly splits a data set into a learning sample and into a test sample that can be selected by the user.
#'@usage
#' plsda.split.sample(data,prop.train=0.75)
#'@param
#' the dataset to be split
#'@param
#' print() function overloading so that it displays samples breakdown by train and test
#'@return
#'{train} a subset matrix of data corresponding to the training sample dataset.
#'
#'{test} a subset matrix of data corresponding to the test sample dataset.
#'@examples

#'@export
plsda.split.sample<-function(data,prop.train){

  # prop.train check
  if(prop.train>1 | prop.train<0){
    stop("Proportion non comprise entre 0 et 1")
  }
  #ok 
  n <- nrow(data)
  #make this example reproductible
  set.seed(1)
  #selection of individuals' index from train sample
  index <- sample(1:n, size = n*prop.train ,replace = FALSE)
  #design of the instance
  instance <- list()
  #train sample
  instance$train <- data[index,]
  # collection of train sample's length 
  instance$train_size <- nrow( instance$train)
  #test sample
  instance$test<- data[-index,]
  #collection of test sample's length 
  instance$test_size <- nrow(instance$test)
  class(instance) <- "split"
  return(instance)

}



#overloading of the native print() function in order to display sample breakdown by train and test
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
