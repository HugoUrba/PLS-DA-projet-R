plsda.predict <- function(objetPLSDA, newdata, type = "class"){
  if (class(objetPLSDA) != "PLSDA") {
    stop("Objet_PLSDA must be a PLSDA class Object")
  }
  
  X <- apply(newdata, 1, function(x) x - colMeans(objetPLSDA$X))
  X <- X/apply(objetPLSDA$X,2, sd)
  Ypred <- t(X) %*% objetPLSDA$coef
  Ypred <- Ypred + colMeans(objetPLSDA$Y)
  Yexp <- apply(Ypred,1, exp)
  Ysoftmax <- t(Yexp/colSums(Yexp))
  if (type == "posterior"){
    return(Ysoftmax)
  }
  else if (type == "class"){
    pred <- apply(Ysoftmax,1,which.max) # max des prob par lignes
    predY <- objetPLSDA$Ymodalities[pred] # nom de la calsse correspondantes
    return(predY)
  }
}