\name{plsda.cv}
\alias{plsda.cv}
\title{Cross Validation for Partial Least Squares Discriminant Analysis}
\usage{
plsda.cv(formula, data, nfold = 5)
}
\arguments{
\item{formula}{an object of class "formula" (or one that can be coerced to that class):
a symbolic description of the model to be fitted.}

\item{nfold}{the number of folds used for cross-validation (k=5 by default).}
}
\value{
\code{ncomp} the number of components that must be used in plsda.fit.
\cr
\code{RESS} a vector containing the calculated PRESS for each components.
\cr
\code{min.RESS}the minimum value of the vector RESS that has been calculated.
}
\description{
This function performs a k-cross-validation in order to determine the number of components \code{ncomp}
to use in \code{plsda.fit} function.
}
\examples{
plsda.cv(Species~., data = iris)
plsda.cv(Species~.,data=iris, nfold = 10)
}
