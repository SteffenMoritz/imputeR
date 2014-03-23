library(impute)
require(MASS)
# Parkinson Data ----------------------------------------------------------
#
tic <- read.delim("./data/ticdata2000.txt", header=FALSE)
data <- tic
Detect(data)
set.seed(1234)
data <- cbind(data[, Detect(data) == "character"], scale(data[, which(Detect(data) == "numeric")]))
simdata <- SimIm(data[, ], 0.1)
sum(is.na(simdata))

Detect(data)
simdata <- SimIm(data, 0.3)
# delete the first and 18th column, they are not numeric predictors
lmfuns <- c("stepBackR", "stepForR", "stepBothR", "lassoR", "pcrR", "plsR",
            "randomForest", "earth", "glmboost", "ridgeR")
cfuns <- c("stepBackC", "stepForC", "stepBothC", "lassoC", "rpartC", "rdaC",
           "randomForest", "ridgeC", "gbmC")

try <- impute(simdata, lmFun = lmfuns[1], cFun = cfuns[1])




# TEST
for(i in seq_along(lmfuns)) {
  for (j in seq_along(cfuns)){
    try <- impute(simdata, lmFun = lmfuns[i], cFun = cfuns[j])
  }
}
try <- impute(simdata, lmFun = lmfuns[8], cFun = cfuns[5])
mixError(try$imp, simdata, data)
base0 <- mixGuess(simdata)

mixError(base0, simdata, data)
