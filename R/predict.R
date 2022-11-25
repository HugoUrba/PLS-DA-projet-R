#' Predict function
#' It is used either to predict the modality of a target variable depending on the explanatory variables.
#' @param
#' objetPLSDA, an object belonging to the class PLSDA, an object which was returned by the fit function. 
#' The model obtained with our train dataset
#' @param 
#' newdata, the test dataset, with a number of observation and values for each explanatory variable. 
#' @param
#' type, the type of prediction we want. If the type is posterior, the function returns 
#' the probability of belonging to each modality.
#' If the type is class, the function returns the modality predicted for each observation.   
#'
#' @return  
#'A column vector containing the predicted values for the target variable, according to the test dataset.
#'
#'@export


plsda.predict <- function(objetPLSDA, newdata, type = "class"){
  if (class(objetPLSDA) != "PLSDA") {
    stop("Objet_PLSDA must be a PLSDA class Object")
  }
  
  X <- apply(newdata, 1, function(x) x - colMeans(objetPLSDA$X))
  X <- X/apply(objetPLSDA$X,2, sd)
  
  Ypred <- t(X) %*% objetPLSDA$coef
  Ypred <- Ypred + colMeans(objetPLSDA$Y)
  
  
  Yexp <- apply(Ypred,1, exp)
  Ysoftmax <- t(Yexp/colSums(Yexp)) #softmax calculs
  
  if (type == "posterior"){
    objet1 <- list(
      "Yprob"=Ysoftmax,
      "Ypred"=Ypred
    )
    return(objet1)
  }
  else if (type == "class"){
    pred <- apply(Ysoftmax,1,which.max) # prob max per line
    classYpred <- objetPLSDA$modalities[pred] # name of the class
    objet2 <- list(
      "predclass"=as.matrix(as.factor(classYpred))
    )
    
    return(objet2)
  }
}
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
pls <- plsda.fit(Species~.,data = split$train)
split <- plsda.split_sample(iris,0.7)
Ypred <- plsda.predict(pls, split$test[1:4], type = "posterior")
