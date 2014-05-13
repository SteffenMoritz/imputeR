require(Hmisc)
source("./code/methods.R")
source("./code/funs.R")
source("./code/impute.R")
# Simulation Experiments --------------------------------------------------
base <- SimEval(data, p = 0.1, guess = TRUE)
lasso <- SimEval(data, p = 0.3, method = "lassoIm")
rf <- SimEval(data, p = 0.3, method = "randomForest")

ridge <- SimEval(data, p = 0.3, method = "ridgeIm", verbose = FALSE)
stepback <- SimEval(data, p = 0.3, method = "stepBackIm", verbose = FALSE)
stepBoth <- SimEval(data, p = 0.3, method = "stepBothIm", verbose = FALSE)
stepfor <- SimEval(data, p = 0.3, method = "stepForIm", verbose = FALSE)
earth <- SimEval(data, p = 0.3, method = "earth", verbose = FALSE)
rrf <- SimEval(data, p = 0.3, method = "RRF", verbose = FALSE)
glmboost <- SimEval(data, p = 0.3, method = "glmboost", verbose = FALSE)

nrmse1 <- cbind(base$nrmse, stepback$nrmse, stepBoth$nrmse, stepfor$nrmse, 
                lasso$nrmse, rf$nrmse, ridge$nrmse, earth$nrmse, rrf$nrmse,
                glmboost$nrmse)
time1 <- c(base$time, stepback$time, stepBoth$time, stepfor$time, 
           lasso$time, rf$time, ridge$time, earth$time, rrf$time,
           glmboost$time)
fc <- function(x) {
  mean(unlist(x$conv))
}
conv1 <- c(0, fc(stepback), fc(stepBoth), fc(stepfor), 
           fc(lasso), fc(rf), fc(ridge), fc(earth), fc(rrf),
           fc(glmboost))
colnames(nrmse1) <- c("base", "stepback", "stepBoth", "stepfor", 
                      "lasso", "rf", "ridge", "earth", "rrf", "glmboost")

mean1 <- apply(nrmse1, 2, mean)
sd1 <- apply(nrmse1, 2, sd)
# pvalue <- numeric(length(mean1))
# for (i in seq_along(pvalue)) {
#   pvalue[i] <- wilcox.test(nrmse1[, i], nrmse1[, 1], alternative = "less")$p.value
# }
results <- cbind(mean1, sd1, time1, conv1)
rownames(results) <- colnames(nrmse1)
table1 <- xtable(results)
digits(table1)[c(2:4)] <- 4
print(table1)
save(base, lasso, rf, ridge, stepback, stepBoth, stepfor, earth, rrf, glmboost, 
     file = "results3.Rdata")
