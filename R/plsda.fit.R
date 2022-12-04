#' PLS-DA
#' {plsda.fit} is used to fit a PLS-DA regression model.plsda.fit(formula, data,ncomp or cv)
#' @param
#' formula, the Y to be correlated to the dataframe(eg:fit<-plsda.fit(Species~.,))
#' @param
#' data, the dataframe containing the variables in the model.
#' @param
#' ncomp, the number of components extracted in NIPALS algorithm, an integer
#' comprised between 1 and the number of explanatory variables. It is fixed at 2 if you do not precise.
#' @return
#' An object of class 'PLSDA' containing the following components :
#' @return
#' {X} the original dataset containing the predictors.
#' 
#' {Y} the variable(s) to predict.
#' 
#' {level} the terms of the variables to predict.
#' 
#' {Xmeans} the means of each predictor.
#' 
#' {pls.coef}the loadings of the predictors, calculated by NIPALS algorithm in plsda.pls function.
#' 
#' {ncomp} the number of components used in plsda.pls.
#' 
#' {locoef} the coefficients of the logistic regression applied on the loadings of the predictors
#' to have the final coefficients of PLS-DA.
#' 
#' {plsda.coef} the final coefficients of the plsda, which are logit functions.

#' @examples
#' fit <-plsda.fit(Species~;, iris, ncomp=3)
#'@export


# LEARNING AND CREATION OF THE PLSDA MODEL
plsda.fit<-function(formula, data, ncomp = 2){
  
  #check whether the input is a formula Y~X
  if(!inherits(formula, "formula")){
    stop("formula must be R formula !")
  }
  
  #collection of the target variable name
  Yname <- intersect(all.vars(formula)[1],colnames(data)) #naming Y column
      
  #getting X et Y
  X <- model.matrix(formula, data = data) #
  X <- X[,-1] #suppression of the intercept
  Y <- model.response(model.frame(formula, data = data)) #
  Y <- as.factor(as.vector(Y)) #
  
  # Check if response variable contains at least 2 levels
  if(nlevels(Y)<2){
  stop("The response variable must contain at least 2 levels")
  }
  
  Xname <- colnames(X) #naming X columns
  
  #calculation of Xmeans
  Xmeans <- colMeans(X)
  
  #definitions of n (nb of observations), p (nb of explanatory variables), q (nb of modalities)
  n <- nrow(X)
  p <- ncol(X)
  q <- nlevels(Y)
  
  #collection of the Y dummies-matrix
  Yb <- as.data.frame(plsda.dummies(Y)$dum)
  
  #scaling of X and Y
  Xk <- scale(X)
  Yk <- scale(Yb)
  
  #design of the following matrices : weight, scores and loadings
  Xweights <- matrix(0, p, ncomp)
  Yweights <- matrix(0, q, ncomp)
  Xscores <- matrix(0, n, ncomp)
  Yscores <- matrix(0, n, ncomp)
  Xloadings <- matrix(0, p, ncomp)
  Yloadings <- matrix(0, q, ncomp)
  
  u <- Yb[,1]
  
  ##design of of a loop to make the Nipals calculations
  for (i in 1:ncomp){
    Wold <- rep(1,p)
    n_iter <- 1
    repeat{
      #Nipals calculs
      W <- t(Xk)%*%u/sum(u^2)
      W <- W/sqrt(sum(W^2))
      t <- Xk%*%W
      q <- t(Yk)%*%t/sum(t^2)
      u <- Yk%*%q/sum(q^2)
      Wdiff <- W-Wold
      if(sum(Wdiff^2) < 1e-10 | n_iter == 500){break}
      Wold <- W
      n_iter <- n_iter+1
    }
    
    #Nipals calculations
    t <- Xk%*%W
    u <- Yk%*%q/sum(q^2)
    Xl <- t(Xk)%*%t/sum(t^2)
    Xk <- Xk-t%*%t(Xl)
    Yl <- t(Yk)%*%t/sum(t^2)
    Yk <- Yk-t%*%t(Yl)
    
    #fill in the matrices
    Xweights[, i] <- W
    Yweights[, i] <- q
    Xscores[, i] <- t
    Yscores[, i] <- u
    Xloadings[, i] <- Xl
    Yloadings[, i] <- Yl
  }
  
  #getting Xrotations for the coefficients
  Xrotations <- Xweights%*%solve(t(Xloadings)%*%Xweights)
  
  #getting the coefficients
  coef <- Xrotations%*%t(Yloadings)
  coef <- coef*sapply(Yb, sd)
  
  #getting the intercept
  intercept <- colMeans(Yb)
  
  #tables names
  compnames <- paste0("Comp.", 1:ncomp)
  
  #naming all names of the rows and columns
  rownames(Xweights) <- Xname
  colnames(Xweights) <- compnames
  rownames(Yweights) <- colnames(Yb)
  colnames(Yweights) <- compnames 
  rownames(Xscores) <- rownames(X)
  colnames(Xscores) <- compnames
  rownames(Yscores) <- rownames(Yb)
  colnames(Yscores) <- compnames
  rownames(Xloadings) <- Xname
  colnames(Xloadings) <- compnames
  rownames(Yloadings) <- colnames (Yb)
  colnames(Yloadings) <- compnames
  rownames(coef) <- Xname
  colnames(coef) <- colnames(Yb)
  
  # Object definition
  objet <- list(
    "X" = X,
    "Y" = Yb,
    "Xname" = Xname,
    "Yname" = Yname,
    "modalities" = levels(Y),
    "Xmeans" = Xmeans,
    "Xweights" = Xweights,
    "Yweigths" = Yweights,
    "Xscores" = Xscores,
    "Yscores" = Yscores,
    "Xloadings" = Xloadings,
    "Yloadings" = Yloadings,
    "coef" = coef,
    "intercept" = intercept,
    "ncomp" = ncomp,
    "components" = compnames,
    "n_iter" = n_iter
  )
  class(objet)<-"PLSDA"
  return(objet)
}

#overload the print function to get a classification
print.PLSDA <- function(objetPLSDA){
  classification <- rbind(objetPLSDA$coef, objetPLSDA$intercept)
  
  cat("classification :","\n")
  print(classification)
}

#overload the summary to get the confusion matrix and the accuracy
summary.PLSDA <- function(objetPLSDA){
  
  #getting the predictions
  objetpred = plsda.predict(objetPLSDA, objetPLSDA$X, type = "class")
  
  #getting the predicted Y
  Y = as.factor(objetPLSDA$modalities[apply(objetPLSDA$Y,1,which.max)])
  
  #creation of the confusion matrix
  mc = table(objetpred, Y)
  
  #getting the accuracy
  accuracy = sum(diag(mc)) / sum(mc)
  print(objetPLSDA)
  
  cat("\nConfusion :","\n")
  print(mc)
  
  cat("\nAccuracy :","\n")
  print(accuracy)
}

