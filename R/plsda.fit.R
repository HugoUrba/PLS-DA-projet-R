#' PLS-DA
#'\code{plsda.fit} is used to fit a PLS-DA regression model.
#' @param
#' formula, an object of class formula: a symbolic description of the model
#' to be fitted. The details of model specification are given under ‘Details'.
#' @param
#' data, the dataframe containing the variables in the model.
#' @param
#' ncomp, the number of components extracted in NIPALS algorithm, an integer
#' comprised between 1 and the number of explanatory variables.
#' @return
#'An object of class 'PLSDA' containing the following components :
#' @return
#' \code{X} the original dataset containing the predictors.
#' \cr
#' \code{Y} the variable(s) to predict.
#' \cr
#' \code{level} the terms of the variables to predict.
#' \cr
#' \code{Xmeans} the means of each predictor.
#' \cr
#' \code{pls.coef}the loadings of the predictors, calculated by NIPALS algorithm in plsda.pls function.
#' \cr
#' \code{ncomp} the number of components used in plsda.pls.
#' \cr
#' \code{locoef} the coefficients of the logistic regression applied on the loadings of the predictors
#' to have the final coefficients of PLS-DA.
#' \cr
#' \code{plsda.coef} the final coefficients of the plsda, which are logit functions.
#' \cr
#' @examples
#' #ncomp is specified
#'fit1<-plsda.fit(Species~.,iris,ncomp=4)

library(fastDummies)
library(plyr)

# APPRENTISSAGE ET CREATION DU MODELE PLSDA
plsda.fit<-function(formula, data, ncomp = 2){
  
  #check if the entry is a formula Y~X
  if(plyr::is.formula(formula)==F){
    stop("formula must be R formula !")
  }
  
  #getting X et Y
  X <- model.matrix(formula, data = data)
  X <- X[,-1] #suppression de l'intercept
  Y <- model.response(model.frame(formula, data = data))
  Y <- as.factor(as.vector(Y))
  
  #Extraction of the target variable name
  Xname <- colnames(X)
  Yname <- intersect(all.vars(formula)[1],colnames(data))
  
  #calculation of Xmeans
  Xmeans <- colMeans(X)
  
  #definition of n (nb of observations), p (nb of explicative variables), q (nb of modalities)
  n <- nrow(X)
  p <- ncol(X)
  q <- nlevels(Y)
  
  #getting the Y modality matrix
  Yb <- dummy_cols(Y)
  Yb <- Yb[,-1]
  colnames(Yb) <- levels(Y)
  
  #scaling X and Y
  Xk <- scale(X)
  Yk <- scale(Yb)
  
  #instanciation des matrics weight, scores et loading de X et Y
  Xweights <- matrix(0, p, ncomp)
  Yweights <- matrix(0, q, ncomp)
  Xscores <- matrix(0, n, ncomp)
  Yscores <- matrix(0, n, ncomp)
  Xloadings <- matrix(0, p, ncomp)
  Yloadings <- matrix(0, q, ncomp)
  
  u <- Yb[,1]
  
  #boucle for afin de remplir les matrices précédentes
  for (i in 1:ncomp){
    Wold <- rep(1,p)
    n_iter <- 1
    repeat{
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
    t <- Xk%*%W
    u <- Yk%*%q/sum(q^2)
    Xl <- t(Xk)%*%t/sum(t^2)
    Xk <- Xk-t%*%t(Xl)
    Yl <- t(Yk)%*%t/sum(t^2)
    Yk <- Yk-t%*%t(Yl)
    
    Xweights[, i] <- W #poids des X
    Yweights[, i] <- q #poids des Y
    Xscores[, i] <- t
    Yscores[, i] <- u
    Xloadings[, i] <- Xl
    Yloadings[, i] <- Yl
  }
  
  Xrotations <- Xweights%*%solve(t(Xloadings)%*%Xweights)
  coef <- Xrotations%*%t(Yloadings)
  coef <- coef*sapply(Yb, sd)
  intercept <- colMeans(Yb)
  
  #tables names
  compnames <- paste0("Comp.", 1:ncomp)
  
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
  # Définition de l'objet
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
    "intercept" = intercept
  )
  class(objet)<-"PLSDA"
  return(objet)
}

print.classi <- function(objetPLSDA){
  classification <- rbind(objetPLSDA$intercept, objetPLSDA$coef)
  
  cat("dumny species ","\n")
  dumny <- as.matrix(objet$dum)
  print(dumny)
  
}
print(object)
