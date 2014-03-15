
#' Best subset for regression (both direction)
#' 
#' Best subset variable selection from both forward and backward
#' direction for continuous data
#' @param x predictor matrix
#' @param y response vector
stepBothR <- function(x, y, ...) {
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <- lm(y~1, data = impdata)
  # the full model with all variables
  full <- lm(y~., data = impdata)
  model <- step(null, scope = list(upper = full), data = impdata,
                trace = 0, direction = "both")
  return(model)
}

#' Best subset (backward direction) for regression
#' 
#' Best subset variable selection (backward direction) for continuous data
#' @param x predictor matrix
#' @param y response vector
stepBackR <- function(x, y, ...) {
  # step back selection for regression
  
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <- lm(y~1, data = impdata)
  # the full model with all variables
  full <- lm(y~., data = impdata)
  model <- step(full, data = impdata, trace = 0, direction = "backward")
  return(model)
}

#' Best subset (forward direction) for regression
#' 
#' Best subset variable selection (forward direction) for continuous data
#' @param x predictor matrix
#' @param y response vector
stepForR <- function(x, y, ...) {
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <- lm(y~1, data = impdata)
  # the full model with all variables
  full <- lm(y~., data = impdata)
  model <- step(null, scope = list(lower = null, upper = full),
                trace = 0, direction = "forward")
  return(model)
}


#' LASSO for regression
#' 
#' LASSO variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
lassoR <- function(x, y) {
  # LASSO for variable selection
  
  lamb <- cv.glmnet(x, y)$lambda.min
  model <- glmnet(x, y, lambda = lamb)
  return(model)
}

#' Ridge shrinkage for regression
#' 
#' Ridge shrinkage variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
ridgeR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- linearRidge(y~., data = impdata)
  return(model)
}

#' Boosting for regression
#' 
#' boosting variable selection for continuous data
#' @param x predictor matrix
#' @param y response vector
glmboostR <- function(x, y, ...) {
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  t.model <- glmboost(y~., data = impdata,
                      control = boost_control(mstop = 1000))
  ms <- mstop(AIC(t.model, method = "corrected"))
  model <- t.model[ms]
  #   browser()
  return(model)
}

#' Principle component regression for imputation
#' 
#' Principle component regression method for imputation
#' @param x predictor matrix
#' @param y response vector
pcrR <- function(x, y) {
  
  impdata <- data.frame(cbind(y, x))
  model <- pcr(y~., data = impdata, ncomp = 2)
  return(model)
}

#' Partial Least Square regression for imputation
#' 
#' Principle component regression method for imputation
#' @param x predictor matrix
#' @param y response vector
plsR <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- plsr(y~., data = impdata, ncomp = 2)
  return(model)
}

#' Cubist method for imputation
#' 
#' Quinlan's Cubist model for imputation
#' @param x predictor matrix
#' @param y response vector
CubistR <- function(x, y) {
  cTune <- train(x = x, y = y, "cubist",
                 tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),
                                        .neighbors = c(0, 1, 5, 9)),
                  trControl = trainControl(method = "cv"))
  model <- cubist(x, y, committees = cTune$bestTune$committees)
  return(list(model = model, neighbors = cTune$bestTune$neighbors))
}