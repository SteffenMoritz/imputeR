library(impute)

# Parkinson Data ----------------------------------------------------------
parkinson <- read.csv("./data/parkinson.csv")
# delete the first and 18th column, they are not numeric predictors
data <- scale(parkinson[, -c(1, 18)])
Detect(data)
set.seed(1234)
simdata <- SimIm(data, 0.1)
# nmissing(simdata)

# Use step with both direction
system.time(impdata_step1 <- impute(simdata, "stepBothR"))
(rmse_step1 <- Rmse(impdata_step1$imp, simdata, data, norm = TRUE))
plotIm(impdata_step1$imp, simdata, data)
conv_step1 <- impdata_step1$conv


# Use step with back direction
system.time(impdata_step2 <- impute(simdata, "stepBackR"))
(rmse_step1 <- Rmse(impdata_step2$imp, simdata, data, norm = TRUE))
plotIm(impdata_step2$imp, simdata, data)
conv_step2 <- impdata_step2$conv

# Use step with forward direction
system.time(impdata_step3 <- impute(simdata, "stepForR"))
(rmse_step1 <- Rmse(impdata_step3$imp, simdata, data, norm = TRUE))
plotIm(impdata_step3$imp, simdata, data)
conv_step3 <- impdata_step3$conv

# Use lasso
system.time(impdata_lasso <- impute(simdata, "lassoR"))
(rmse_lasso <- Rmse(impdata_lasso$imp, simdata, data, norm = TRUE))
plotIm(impdata_lasso$imp, simdata, data)
conv_lasso <- impdata_lasso$conv


# Use Ridge regression
system.time(impdata_ridge <- impute(simdata, "ridgeR"))
(rmse_ridge <- Rmse(impdata_ridge$imp, simdata, data, norm = TRUE))
plotIm(impdata_ridge$imp, simdata, data)
conv_ridge <- impdata_ridge$conv


# Use MARS
system.time(impdata_mars <- impute(simdata, "earth"))
(rmse_mars <- Rmse(impdata_mars$imp, simdata, data, norm = TRUE))
plotIm(impdata_mars$imp, simdata, data)
conv_mars <- impdata_mars$conv

# Use gamboost
system.time(impdata_mars <- impute(simdata, "gamboostR"))
(rmse_mars <- Rmse(impdata_mars$imp, simdata, data, norm = TRUE))
plotIm(impdata_mars$imp, simdata, data)
conv_mars <- impdata_mars$conv

# Use Random Forests
system.time(impdata_rf <- impute(simdata, "randomForest"))
(rmse_rf <- Rmse(impdata_rf$imp, simdata, data, norm = TRUE))
plotIm(impdata_rf$imp, simdata, data)
conv_rf <- impdata_rf$conv

# Use regularised Random Forests
system.time(impdata_rrf <- impute(simdata, "RRF"))
(rmse_rrf <- Rmse(impdata_rrf$imp, simdata, data, norm = TRUE))
plotIm(impdata_rrf$imp, simdata, data)
conv_rrf <- impdata_rrf$conv


# use PCR
system.time(impdata_pcr <- impute(simdata, "pcrR"))
(rmse_pcr <- Rmse(impdata_pcr$imp, simdata, data, norm = TRUE))

# use PLS
system.time(impdata_pls <- impute(simdata, "plsR"))
(rmse_pls <- Rmse(impdata_pls$imp, simdata, data, norm = TRUE))

# Use Cubist
require(Cubist)
system.time(impdata_cubist <- impute(simdata, "CubistR"))
(rmse_cubist <- Rmse(impdata_cubist, simdata, data, norm = TRUE))
plotIm(impdata_cubist, simdata, data)

# Compare to some other methods -------------------------------------------
require(imputation)
(rmse_mean <- otherIm(meanImpute))
rmse_gbm <- otherIm(gbmImpute)
rmse_knn <- otherIm(kNNImpute, k = 5)
rmse_lm <- otherIm(lmImpute)
rmse_svd <- otherIm(SVDmiss)
