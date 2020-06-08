# Stats 506, Fall 2019
# Group Project
#
# This script contains function for cross-validation to find lambda with minimum
# MSE using parallel computing. Package "doMC" is needed.
#
# Author: Wenjing Zhou (wenjzh@umich.edu)
# Date: December 10, 2019
#80: ---------------------------------------------------------------------------

# libraries: -------------------------------------------------------------------
install.packages("https://cran.r-project.org/src/contrib/doMC_1.3.6.tar.gz",
                 repos = NULL, type = "source")
require(doMC)
doMC::registerDoMC(cores=4)
library(glmnet)

cv_lambda = function (x, y, nfolds = 10) {
  # Description: cross-validation to find lambda with minimum
  # MSE using parallel computing
  #
  # Input:
  # x - predictor matrix
  # y - response matrix
  # nfolds - number of folds;default is 10
  #
  # Output: lambda with minimum MSE 
  N = nrow(x)
  weights = rep(1, N)
  object = glmnet(x, y, weights = weights,
                         family="gaussian",penalty.factor=pnfc,alpha = 1)
  type.measure = cvtype("mse", class(object)[[1]])
  foldid = sample(rep(seq(nfolds), length = N))
  outlist = as.list(seq(nfolds))
  outlist = foreach(i = seq(nfolds), .packages = c("glmnet")) %dopar% 
    {
      which = foldid == i
      if (length(dim(y)) > 1) 
        y_sub = y[!which, ]
      else y_sub = y[!which]
      offset_sub = NULL
      glmnet(x[!which, , drop = FALSE], y_sub, 
             offset = offset_sub, weights = weights[!which], 
             family="gaussian",penalty.factor=pnfc,alpha = 1)
    }
  lambda = object$lambda
  cvstuff = do.call("cv.elnet",list(outlist,lambda,x,y,weights,offset = NULL,
                                     foldid, type.measure, grouped = TRUE,
                                     keep = FALSE, alignment = "lambda"))
  cvm = cvstuff$cvm
  lambda.min=lambda[which.min(cvm)]
  
  #output
  object_cv = list(lambda = lambda, mse=cvm, lambda.min)
  object_cv
}
  