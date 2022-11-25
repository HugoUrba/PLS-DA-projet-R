# PLS-DA-projet-R with R Shiny Dasboard
R package programming qui permet d'effectuer une régression PLS sur un jeu de données

**Deliverables:**

* Design of a DA-Partial Least Squares Regression (PLSDA) algorithm with overload of S3 functions (eg.:print)

* Descriptive and Explanatory Graphs with RShiny Dashboard

* A detailed tutorial (cf.Wiki in the middle of Github-project navigation bar, at the top of the screen)


**Partial Least Squares(PLS) in a few bullet-points **

* a statistical method that bears some relation to principal components regression; instead of finding hyperplanes of minimum variance between the response and independent variables, it finds a linear regression model by projecting the predicted variables and the observable variables to a new space. 

* Because both the X and Y data are projected to new spaces, the PLS family of methods are known as bilinear factor models. 

* Partial least squares Discriminant Analysis (PLS-DA) is a variant used when the Y is categorical.

* PLS regression, like PCA, seeks to find components which maximize the variability of predictors but differs from PCA as PLS requires the components to have maximum correlation with the response. PLS is a supervised procedure whereas PCA is unsupervised.

* In order to explain Y variables from X explanatory variables, the descriptors are summarized into a series of 2X2 orthogonal factors  th  (factorial axes, latent variables, X-scores). As well, Y variables / targets to be explained and predicted are summarized into a series of components uh (scores Y).
During the modeling process, the PLS regression will construct the series of factors (uh, th) so that their covariance is maximum. The number of factors cannot exceed the number of explanatory variables. 
