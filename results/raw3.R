library(impute)
require(MASS)
# Parkinson Data ----------------------------------------------------------
#
tic <- read.delim("./data/ticdata2000.txt", header=FALSE)
data <- tic
Detect(data)
data <- cbind(data[, Detect(data) == "character"], scale(data[, which(Detect(data) == "numeric")]))
set.seed(1234)
simdata <- SimIm(data, 0.1)
simdata3 <- SimIm(data, 0.3)


sum(is.na(simdata))
sum(is.na(simdata3))
Detect(data)
set.seed(1234)
simdata <- SimIm(data, 0.1)
# delete the first and 18th column, they are not numeric predictors
lmfuns <- c("stepBackR", "stepForR", "stepBothR", "lassoR", "pcrR", "plsR",
            "randomForest", "earth", "glmboost", "ridgeR")
cfuns <- c("stepBackC", "stepForC", "stepBothC", "lassoC", "rpartC", "rdaC",
           "randomForest", "ridgeC", "gbmC")
 
try <- impute(simdata, lmFun = lmfuns[8], cFun = cfuns[5])




# TEST
for(i in seq_along(lmfuns)) {
  for (j in seq_along(cfuns)){
    try <- impute(simdata, lmFun = lmfuns[i], cFun = cfuns[j])
  }
}
system.time(try <- impute(simdata, lmFun = lmfuns[8], cFun = cfuns[5]))
mixError(try$imp, simdata, data)
base0 <- mixGuess(simdata)
mixError(base0, simdata, data)

#
p = 0.3
n.sim = 100
base3 <- SimEval(data, n.sim = n.sim, p = p, guess = TRUE)
try3 <- SimEval(data, n.sim = n.sim, p = p, method = c(lmfuns[8], cfuns[5]))
colMeans(try3$error)
colMeans(base3$error)

apply(try3$error, 2, sd)
apply(base3$error, 2, sd)
wilcox.test(base3$error[, 1], try3$error[, 1])
wilcox.test(base3$error[, 2], try3$error[, 2])
