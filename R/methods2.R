#' Best subset for classification (both direction)
#' 
#' Best subset variable selection from both forward and backward
#' direction for categorical data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom stats step
#' @seealso \code{\link{step}}, \code{\link{stepBothR}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "stepBothC")
#' }
stepBothC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- glm(factor(y)~1, data = impdata, family = binomial)
  full <- glm(factor(y)~., data = impdata, family = binomial)
  model <- step(null, scope = list(upper = full), data = impdata,
                trace = 0, direction = "both")
  return(model)
}

#' Best subset for classification (backward)
#' 
#' Best subset variable selection from both forward and backward
#' direction for categorical data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom stats step
#' @seealso \code{\link{step}}, \code{\link{stepBackR}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "stepBackC")
#' }
stepBackC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- glm(factor(y)~1, data = impdata, family = binomial)
  full <- glm(factor(y)~., data = impdata, family = binomial)
  model <- step(full, data = impdata, trace = 0, direction = "backward")
  return(model)
}


#' Best subset for classification (forward direction)
#' 
#' Best subset variable selection from both forward and backward
#' direction for categorical data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom stats step
#' @seealso \code{\link{step}}, \code{\link{stepForR}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "stepForC")
#' }
stepForC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <-glm(factor(y)~1, data = impdata, family = binomial)
  # the full model with all variables
  full <- glm(factor(y)~., data = impdata, family = binomial)
  model <- step(null, scope = list(lower = null, upper = full),
                trace = 0, direction = "forward")
  return(model)
}

#' classification tree for imputation
#' 
#' classification tree for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom rpart rpart
#' @seealso \code{\link{rpart}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "rpartC")
#' }
rpartC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- rpart(y~., data = impdata, method = "class")
  return(model)
}

#' regularised LDA method for imputation
#' 
#' regularised LDA method for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom rda rda
#' @importFrom rda rda.cv
#' @seealso \code{\link{rda}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "rdaC")
#' }
rdaC <- function(x, y) {
  y <- as.numeric(y)
  x <- t(x)
  fit <- rda(x, y)
  cv <- rda.cv(fit, x, y)
  index <- arrayInd(which.min(cv$cv.err), dim(cv$cv.err))
  cv.alpha <- cv$alpha[index[1]]
  cv.delta <- cv$delta[index[2]]
  return(list(x = x , y = y, fit = fit, alpha = cv.alpha, delta = cv.delta))
}

#' logistic regression with lasso for imputation
#' 
#' logistic regression with lasso for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @import glmnet
#' @seealso \code{\link{cv.glmnet}} and \code{\link{glmnet}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "lassoC")
#' }
lassoC <- function(x, y) {
  lamb <- cv.glmnet(x, y, family = "binomial", type.measure = "class")$lambda.min
  model <- glmnet(x, y, family = "binomial", lambda = lamb)
  return(model)
}

#' Ridge regression with lasso for imputation
#' 
#' Ridge regression with lasso for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom ridge logisticRidge
#' @seealso \code{\link{logisticRidge}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "ridgeC")
#' }
ridgeC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- logisticRidge(y~., data = impdata)
  return(model)
}


#' boosting tree for imputation
#' 
#' boosting tree for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' and the best.iter for gbm model.
#' @importFrom gbm gbm
#' @importFrom gbm gbm.perf
#' @seealso \code{\link{gbm}}
#' @export
#' @examples
#' data(spect)
#' missdata <- SimIm(spect, 0.1)
#' \dontrun{
#' impdata <- impute(spect, cFun = "gbmC")
#' }
gbmC <- function(x, y) {
  y <- as.numeric(y)
  impdata <- data.frame(cbind(y, x))
  gbm1 <- gbm(y~., data = impdata, distribution = "adaboost", n.trees = 1000, 
              shrinkage = 0.05, interaction.depth = 1, bag.fraction = 0.5,
              train.fraction = 0.5, cv.folds = 10)
  best.iter <- gbm.perf(gbm1, method = "cv")
  return(list(model = gbm1, best = best.iter))
}