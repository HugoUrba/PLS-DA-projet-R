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
  
  #Creating X by applying the function x
  X <- apply(newdata, 1, function(x) x - colMeans(objetPLSDA$X))
  X <- X/apply(objetPLSDA$X,2, sd)
  
  #calcus to get the softmax function
  Ypred <- t(X) %*% objetPLSDA$coef
  Ypred <- Ypred + colMeans(objetPLSDA$Y)
  
  #apply the exponential function
  Yexp <- apply(Ypred,1, exp)
  
  #softmax calculs
  Ysoftmax <- t(Yexp/colSums(Yexp))
  
  #for the type posterior, returning the probabality of being for each modality
  if (type == "posterior"){
    objet1 <- list(
      "Yprob"=Ysoftmax,
      "Ypred"=Ypred
    )
    return(objet1)
  }
  
  #for the type class
  else if (type == "class"){
    
    #putting one to the modality column with the highest probability
    pred <- apply(Ysoftmax,1,which.max)
    
    #replace the ones with the modality name
    classYpred <- objetPLSDA$modalities[pred] 
    
    #getting the return as a factor
    return(as.factor(classYpred))
  }
}
