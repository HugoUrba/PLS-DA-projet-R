#' Cross Validation for Partial Least Squares Discriminant Analysis
#'
#' This function performs a k-cross-validation in order to determine the number of components \code{ncomp}
#' to use in \code{plsda.fit} function.
#' @param
#' formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted.
#' @param
#' nfold the number of folds used for cross-validation (k=5 by default).
#' @return
#' \code{ncomp} the number of components that must be used in plsda.fit.
#' \cr
#' \code{RESS} a vector containing the calculated PRESS for each components.
#' \cr
#' \code{min.RESS}the minimum value of the vector PRESS that has been calculated.
#' @examples
#' plsda.cv(Species~., data = iris)
#' plsda.cv(Species~.,data=iris, nfold = 5)

#création de la fct S3 cross validation
plsda.cv <- function (formula,data,nfold=5){
  #controle de la classe formula
  if(plyr::is.formula(formula)== FALSE ){
    print("formula must be R formula")
  }
  # controle du data: dataframe or matrix
  ok <- (is.data.frame(data) | is.matrix(data))
  if (!ok){
    stop("data should be a dataframe or a matrix")
  }
  #Diviser le jeu de données en un ensemble de données (X) et une variable cible (y)
  X <- model.matrix(formula, data = data)
  X <- X[,-1] #suppression de l'intercept
  Y <- model.response(model.frame(formula, data = data))
  #nombre d'observations dans chaque Fold
  n <- trunc(nrow(X)/nfold)
  #reproduire le meme resultat
  set.seed(1)
  s<-sample(1:nrow(X),nrow(X))

  newX <- X[s,]
  newY <- Y[s]
  #nombre de composantes par défaut est le rang de la matrice X
  ncomp <- min(nrow(X), ncol(X))
  #creation de l'instance
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
    #calcul du RESS pour chaque composante
    instance$RESS[j] <-as.numeric(sum(instance$RESS[i]))
  }
  #choisir le ncomp qui correspond au min de RESS
  instance$ncomp <- which.min(instance$RESS)
  #retourner le RESS correspondant
  instance$RESS <- instance$RESS[ instance$ncomp]
  class(instance) <- "cross_validation"
  return(instance)
}

