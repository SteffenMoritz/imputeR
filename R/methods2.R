#' Best subset for classification (both direction)
#' 
#' Best subset variable selection from both forward and backward
#' direction for categorical data
#' @param x predictor matrix
#' @param y response vector
stepBothC <- function(x, y, ...) {
  # stepwise both for classification
  
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <- glm(factor(y)~1, data = impdata, family = binomial)
  # the full model with all variables
  full <- glm(factor(y)~., data = impdata, family = binomial)
  model <- step(null, scope = list(upper = full), data = impdata,
                trace = 0, direction = "both")
  return(model)
}

stepBackC <- function(x, y, ...) {
  # step back selection for classification
  
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <- glm(factor(y)~1, data = impdata, family = binomial)
  # the full model with all variables
  full <- glm(factor(y)~., data = impdata, family = binomial)
  model <- step(full, data = impdata, trace = 0, direction = "backward")
  return(model)
}

stepForC <- function(x, y, ...) {
  # step forward selection for classification
  
  impdata <- data.frame(cbind(y, x))
  # the null model with only the intercept
  null <-glm(factor(y)~1, data = impdata, family = binomial)
  # the full model with all variables
  full <- glm(factor(y)~., data = impdata, family = binomial)
  model <- step(null, scope = list(lower = null, upper = full),
                trace = 0, direction = "forward")
  return(model)
}

# rpart
rpartC <- function(x, y) {
  impdata <- data.frame(cbind(y, x))
  model <- rpart(y~., data = impdata, method = "class")
  return(model)
}

# rda
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

# logistic lasso
lassoC <- function(x, y) {
  # LASSO for logistic regression
  
  lamb <- cv.glmnet(x, y, family = "binomial", type.measure = "class")$lambda.min
  model <- glmnet(x, y, family = "binomial", lambda = lamb)
  return(model)
}

# logistic ridge
ridgeC <- function(x, y) {
  # ridge regression, lambda is chosen automatically
  
  impdata <- data.frame(cbind(y, x))
  model <- logisticRidge(y~., data = impdata)
  return(model)
}

# gbm

gbmC <- function(x, y) {
  y <- as.numeric(y)
  impdata <- data.frame(cbind(y, x))
  gbm1 <- gbm(y~., data = impdata, distribution = "adaboost", n.trees = 1000, 
              shrinkage = 0.05, interaction.depth = 1, bag.fraction = 0.5,
              train.fraction = 0.5, cv.folds = 10)
  best.iter <- gbm.perf(gbm1, method = "cv")
  return(list(model = gbm1, best = best.iter))
}