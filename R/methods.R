#' @title Best subset for regression (both direction)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for continuous data
#' 
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom stats step
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "stepBothR")
#' }
stepBothR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- lm(y~1, data = impdata)
  full <- lm(y~., data = impdata)
  model <- step(null, scope = list(upper = full), data = impdata,
                trace = 0, direction = "both")
  return(model)
}

#' @title Best subset (backward direction) for regression
#' 
#' @description Best subset variable selection (backward direction) for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom stats step
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "stepBackR")
#' }
stepBackR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- lm(y~1, data = impdata)
  full <- lm(y~., data = impdata)
  model <- step(full, data = impdata, trace = 0, direction = "backward")
  return(model)
}

#' @title Best subset (forward direction) for regression
#' 
#' @description Best subset variable selection (forward direction) for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @importFrom stats step
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "stepForR")
#' }
stepForR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  null <- lm(y~1, data = impdata)
  full <- lm(y~., data = impdata)
  model <- step(null, scope = list(lower = null, upper = full),
                trace = 0, direction = "forward")
  return(model)
}


#' @title LASSO for regression
#' 
#' @description LASSO variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @import glmnet
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "lassoR")
#' }
lassoR <- function(x, y) {
  lamb <- cv.glmnet(x, y)$lambda.min
  model <- glmnet(x, y, lambda = lamb)
  return(model)
}

#' @title Ridge shrinkage for regression
#' 
#' @description Ridge shrinkage variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' 
#' @importFrom ridge linearRidge
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "ridgeR")
#' }
ridgeR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- linearRidge(y~., data = impdata)
  return(model)
}

#' @title Boosting for regression
#' 
#' @description boosting variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @import mboost
#' @importFrom stats AIC
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "glmboostR")
#' }
glmboostR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  t.model <- glmboost(y~., data = impdata,
                      control = boost_control(mstop = 1000))
  ms <- mstop(AIC(t.model, method = "corrected"))
  model <- t.model[ms]
  return(model)
}

#' @title Principle component regression for imputation
#' 
#' @description Principle component regression method for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' 
#' @importFrom pls pcr
#' @seealso \code{\link{pcr}}
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "pcrR")
#' }
pcrR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- pcr(y~., data = impdata, ncomp = 2)
  return(model)
}

#' @title Partial Least Square regression for imputation
#' 
#' @description Principle component regression method for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' @seealso \code{\link{plsr}}
#' 
#' @importFrom pls plsr
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "plsR")
#' }
plsR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- plsr(y~., data = impdata, ncomp = 2)
  return(model)
}

#' @title Cubist method for imputation
#' 
#' @description Quinlan's Cubist model for imputation
#' @param x predictor matrix
#' @param y response vector
#' @return a model object that can be used by the \code{\link{impute}} function
#' and the optimal value for the "neighbors".
#' 
#' @importFrom caret train
#' @importFrom caret trainControl
#' @seealso \code{\link{cubist}}
#' @importFrom Cubist cubist
#' 
#' @export
#' @examples
#' data(parkinson)
#' missdata <- SimIm(parkinson, 0.1)
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "CubistR")
#' }
CubistR <- function(x, y) {
  cTune <- train(x = x, y = y, "cubist",
                 tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),
                                        .neighbors = c(0, 1, 5, 9)),
                 trControl = trainControl(method = "cv"))
  model <- cubist(x, y, committees = cTune$bestTune$committees)
  return(list(model = model, neighbors = cTune$bestTune$neighbors))
}

#' @title Best subset for classification (both direction)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for categorical data
#' 
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

#' @title Best subset for classification (backward)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for categorical data
#' 
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


#' @title Best subset for classification (forward direction)
#' 
#' @description Best subset variable selection from both forward and backward
#' direction for categorical data
#' 
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

#' @title classification tree for imputation
#' 
#' @description classification tree for imputation
#' 
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

#' @title regularised LDA method for imputation
#' 
#' @description regularised LDA method for imputation
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

#' @title logistic regression with lasso for imputation
#' 
#' @description logistic regression with lasso for imputation
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

#' @title Ridge regression with lasso for imputation
#' 
#' @description Ridge regression with lasso for imputation
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


#' @title boosting tree for imputation
#' 
#' @description boosting tree for imputation
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