#' @title Best subset for regression (both direction)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for continuous data
#' 
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @importFrom stats step lm
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "stepBothR")
#' }
stepBothR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- stats::lm(y~1, data = impdata)
  full <- stats::lm(y~., data = impdata)
  model <- stats::step(null, scope = list(upper = full), data = impdata,
                trace = 0, direction = "both")
  return(model)
}

#' @title Best subset (backward direction) for regression
#' 
#' @description Best subset variable selection (backward direction) for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @importFrom stats step lm
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "stepBackR")
#' }
stepBackR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- stats::lm(y~1, data = impdata)
  full <- stats::lm(y~., data = impdata)
  model <- stats::step(full, data = impdata, trace = 0, direction = "backward")
  return(model)
}

#' @title Best subset (forward direction) for regression
#' 
#' @description Best subset variable selection (forward direction) for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @importFrom stats step lm
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "stepForR")
#' }
stepForR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- stats::lm(y~1, data = impdata)
  full <- stats::lm(y~., data = impdata)
  model <- stats::step(null, scope = list(lower = null, upper = full),
                trace = 0, direction = "forward")
  return(model)
}


#' @title LASSO for regression
#' 
#' @description LASSO variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @import glmnet
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "lassoR")
#' }
lassoR <- function(x, y) {
  #Check because glmnet is just in Suggests
  if (!requireNamespace("glmnet", quietly = TRUE))
  {
    stop("Package \"glmnet\" needed for lassoR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"glmnet\")",
         call. = FALSE)
  }
  lamb <- glmnet::cv.glmnet(x, y)$lambda.min
  model <- glmnet::glmnet(x, y, lambda = lamb)
  return(model)
}

#' @title Ridge shrinkage for regression
#' 
#' @description Ridge shrinkage variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' 
# #' @importFrom ridge linearRidge
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "ridgeR")
#' }
ridgeR <- function(x, y) {
  #Check because ridge is just in Suggests
  if (!requireNamespace("ridge", quietly = TRUE))
  {
    stop("Package \"ridge\" needed for ridgeR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"ridge\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- ridge::linearRidge(y~., data = impdata)
  return(model)
}

#' @title Boosting for regression
#' 
#' @description boosting variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @import mboost
#' @importFrom stats AIC
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "glmboostR")
#' }
glmboostR <- function(x, y) {
  #Check because glmnet is just in Suggests
  if (!requireNamespace("mboost", quietly = TRUE))
  {
    stop("Package \"mboost\" needed for glmboostR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"mboost\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  t.model <- mboost::glmboost(y~., data = impdata,
                      control = mboost::boost_control(mstop = 1000))
  ms <- mboost::mstop(AIC(t.model, method = "corrected"))
  model <- t.model[ms]
  return(model)
}

#' @title Principle component regression for imputation
#' 
#' @description Principle component regression method for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' 
# #' @importFrom pls pcr
#' @seealso \code{\link[pls]{pcr}}
#' @export 
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "pcrR")
#' }
pcrR <- function(x, y) {
  #Check because pls is just in Suggests
  if (!requireNamespace("pls", quietly = TRUE))
  {
    stop("Package \"pls\" needed for pcrR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"pls\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- pls::pcr(y~., data = impdata, ncomp = 2)
  return(model)
}

#' @title Partial Least Square regression for imputation
#' 
#' @description Principle component regression method for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @seealso \code{\link[pls]{plsr}}
#' 
# #' @importFrom pls plsr
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "plsR")
#' }
plsR <- function(x, y) {
  #Check because pls is just in Suggests
  if (!requireNamespace("pls", quietly = TRUE))
  {
    stop("Package \"pls\" needed for plsR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"pls\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- pls::plsr(y~., data = impdata, ncomp = 2)
  return(model)
}

#' @title Cubist method for imputation
#' 
#' @description Quinlan's Cubist model for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' and the optimal value for the "neighbors".
#' 
# #' @importFrom caret train
# #' @importFrom caret trainControl
#' @seealso \code{\link[Cubist]{cubist}} 
# #' @importFrom Cubist cubist
#' 
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \donttest{
#' impdata <- impute(missdata, lmFun = "CubistR")
#' }
CubistR <- function(x, y) {
  #Check because caret is just in Suggests
  if (!requireNamespace("caret", quietly = TRUE))
  {
    stop("Package \"caret\" needed for CubistR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"caret\")",
         call. = FALSE)
  }
  #Check because Cubist is just in Suggests
  if (!requireNamespace("Cubist", quietly = TRUE))
  {
    stop("Package \"Cubist\" needed for CubistR to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"Cubist\")",
         call. = FALSE)
  }
  cTune <- caret::train(x = x, y = y, "cubist",
                 tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),
                                        .neighbors = c(0, 1, 5, 9)),
                 trControl = caret::trainControl(method = "cv"))
  model <- Cubist::cubist(x, y, committees = cTune$bestTune$committees)
  return(list(model = model, neighbors = cTune$bestTune$neighbors))
}

#' @title Best subset for classification (both direction)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for categorical data
#' 
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @importFrom stats step glm
#' @seealso \code{\link[stats]{step}}, \code{\link[imputeR]{stepBothR}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "stepBothC")
#' }
stepBothC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- stats::glm(factor(y)~1, data = impdata, family = binomial)
  full <- stats::glm(factor(y)~., data = impdata, family = binomial)
  model <- stats::step(null, scope = list(upper = full), data = impdata,
                trace = 0, direction = "both")
  return(model)
}

#' @title Best subset for classification (backward)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for categorical data
#' 
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @importFrom stats step glm
#' @seealso \code{\link[stats]{step}}, \code{\link[imputeR]{stepBackR}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "stepBackC")
#' }
stepBackC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- stats::glm(factor(y)~1, data = impdata, family = binomial)
  full <- stats::glm(factor(y)~., data = impdata, family = binomial)
  model <- stats::step(full, data = impdata, trace = 0, direction = "backward")
  return(model)
}


#' @title Best subset for classification (forward direction)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for categorical data
#' 
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' @importFrom stats step glm
#' @seealso \code{\link[stats]{step}}, \code{\link[imputeR]{stepForR}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "stepForC")
#' }
stepForC <- function(x, y) {
  
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <-stats::glm(factor(y)~1, data = impdata, family = binomial)
  # the full model with all variables
  full <- stats::glm(factor(y)~., data = impdata, family = binomial)
  model <- stats::step(null, scope = list(lower = null, upper = full),
                trace = 0, direction = "forward")
  return(model)
}

#' @title classification tree for imputation
#' 
#' @description classification tree for imputation
#' 
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @importFrom rpart rpart
#' @seealso \code{\link[rpart]{rpart}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "rpartC")
#' }
rpartC <- function(x, y) {
  
  #Check because rpart is just in Suggests
  if (!requireNamespace("rpart", quietly = TRUE))
  {
    stop("Package \"rpart\" needed for rpartC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"rpart\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- rpart::rpart(y~., data = impdata, method = "class")
  return(model)
}


#' @title logistic regression with lasso for imputation
#' 
#' @description logistic regression with lasso for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @import glmnet
#' @seealso \code{\link[glmnet]{cv.glmnet}} and \code{\link[glmnet]{glmnet}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "lassoC")
#' }
lassoC <- function(x, y) {
  
  #Check because glmnet is just in Suggests
  if (!requireNamespace("glmnet", quietly = TRUE))
  {
    stop("Package \"glmnet\" needed for lassoC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"glmnet\")",
         call. = FALSE)
  }
  lamb <- glmnet::cv.glmnet(x, y, family = "binomial", type.measure = "class")$lambda.min
  model <- glmnet::glmnet(x, y, family = "binomial", lambda = lamb)
  return(model)
}

#' @title Ridge regression with lasso for imputation
#' 
#' @description Ridge regression with lasso for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
# #' @importFrom ridge logisticRidge
#' @seealso \code{\link[ridge]{logisticRidge}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "ridgeC")
#' }
ridgeC <- function(x, y) {
  
  #Check because ridge is just in Suggests
  if (!requireNamespace("ridge", quietly = TRUE))
  {
    stop("Package \"ridge\" needed for ridgeC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"ridge\")",
         call. = FALSE)
  }
  impdata <- data.frame(cbind(y, x))
  model <- ridge::logisticRidge(y~., data = impdata)
  return(model)
}


#' @title boosting tree for imputation
#' 
#' @description boosting tree for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link[imputeR]{impute}} function
#' and the best.iter for gbm model.
# #' @importFrom gbm gbm
# #' @importFrom gbm gbm.perf
#' @seealso \code{\link[gbm]{gbm}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \donttest{
#' impdata <- impute(spect, cFun = "gbmC")
#' }
gbmC <- function(x, y) {
  
  #Check because gbm is just in Suggests
  if (!requireNamespace("gbm", quietly = TRUE))
  {
    stop("Package \"gbm\" needed for gbmC to work. Try to install it and then run your code again. You can install the package by executing: install.packages(\"gbm\")",
         call. = FALSE)
  }
  y <- as.numeric(y)
  impdata <- data.frame(cbind(y, x))
  gbm1 <- gbm::gbm(y~., data = impdata, distribution = "adaboost", n.trees = 1000, 
              shrinkage = 0.05, interaction.depth = 1, bag.fraction = 0.5,
              train.fraction = 0.5, cv.folds = 10)
  best.iter <- gbm::gbm.perf(gbm1, method = "cv")
  return(list(model = gbm1, best = best.iter))
}