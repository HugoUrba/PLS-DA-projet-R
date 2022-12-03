#' Cross Validation for Partial Least Squares Discriminant Analysis
#'
#' This function performs a k-cross-validation in order to determine the right number of components \code{ncomp}
#' to use in \code{plsda.fit} function.
#' @param
#' formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' @param
#' nfold the number of folds used for cross-validation (k=5 by default).
#' @return
#' \code{ncomp} the number of components that should be used in plsda.fit.
#' \cr
#' \code{RESS} a vector containing the calculated RESS for each components.
#' \cr
#' \code{min.RESS}the minimum value of the vector PRESS that has been calculated.
#' @examples
#' plsda.cv(Species~., data = iris)
#' plsda.cv(Species~.,data=iris, nfold = 5)

#design of the S3 cross-validation function
plsda.cv <- function (formula,data,nfold=5){
  #check of the formula class
  if(plyr::is.formula(formula)== FALSE ){
    print("formula must be R formula")
  }
  #check of the data: dataframe or matrix
  ok <- (is.data.frame(data) | is.matrix(data))
  if (!ok){
    stop("data should be a dataframe or a matrix")
  }
  #split of the dataset into a set of data X and a target-variable (y)
  X <- model.matrix(formula, data = data)
  X <- X[,-1] ##removal of the intercept
  Y <- model.response(model.frame(formula, data = data))
  #number of observations in each fold 
  n <- trunc(nrow(X)/nfold)
  #reproducing the same output
  set.seed(1)
  s<-sample(1:nrow(X),nrow(X))

  newX <- X[s,]
  newY <- Y[s]
  #number of components : per default = matrix X rank
  ncomp <- min(nrow(X), ncol(X))
  #design of the instance
  instance <- list()
  for(j in 1:ncomp){
    for (i in (1:nfold)){
      #index du ième échantillon pour les données de validation
      instance$idx <- c((1+(i-1)*n):(n*(i)))
      #Fournir les indices d'entraînement /validation pour diviser les données en ensembles d'entraînement/validation.
      #train data
      instance$x.train <- newX[-instance$idx ,]
      instance$y.train <- newY[-instance$idx ]
      instance$train <- data.frame("Y"=instance$y.train,instance$x.train)
      #validation data
      instance$x.val <- newX[instance$idx ,]
      instance$y.val<- dummy_cols(newY)
      instance$y.val <- instance$y.val[instance$idx ,]
      #entrainer le modèle sur les données d'apprentissage
      instance$fit <-plsda.fit(instance$y.train~., instance$train, ncomp = j)
      #tester sur les données de test
      instance$predict <- plsda.predict( instance$fit,instance$x.val,type="class")
      #calcul du residual sum of Squares pour le i ème échantillon
      instance$RESS[i] <- mean(instance$y.val != instance$predict)
    }
    #calculation of the RESS for each component
    instance$RESS[j] <-as.numeric(sum(instance$RESS[i]))
  }
  #choice of the ncomp with the lowest RESS
  instance$ncomp <- which.min(instance$RESS)
  #return the corresponding RESS
  instance$RESS <- instance$RESS[ instance$ncomp]
  class(instance) <- "cross_validation"
  return(instance)
}

