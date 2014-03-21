library(impute)
require(MASS)
# Parkinson Data ----------------------------------------------------------
data <- Boston
# house <- read.csv("./data/housing.csv")

Detect(data)
simdata <- SimIm(data, 0.3)
# delete the first and 18th column, they are not numeric predictors
lmfuns <- c("stepBackR", "stepForR", "stepBothR", "lassoR", "pcrR", "plsR",
            "randomForest", "earth", "glmboost", "ridgeR")
cfuns <- c("stepBackC", "stepForC", "stepBothC", "lassoC", "rpartC", "rdaC",
           "randomForest", "gbmC", "ridgeC")

try <- imputee(simdata, lmFun = lmfuns[1], cFun = cfuns[1])
