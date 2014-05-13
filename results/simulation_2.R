require(Hmisc)

# Simulation Experiments --------------------------------------------------
base <- SimEval(data, guess = TRUE)
lasso <- SimEval(data, ini = "random", method = "lassoIm")
rf <- SimEval(data, ini = "random", method = "randomForest")

ridge <- SimEval(data, ini = "random", method = "ridgeIm", verbose = FALSE)
stepback <- SimEval(data, ini = "random", method = "stepBackIm", verbose = FALSE)
stepBoth <- SimEval(data, ini = "random", method = "stepBothIm", verbose = FALSE)
stepfor <- SimEval(data, ini = "random", method = "stepForIm", verbose = FALSE)
earth <- SimEval(data, ini = "random", method = "earth", verbose = FALSE)
rrf <- SimEval(data, ini = "random", method = "RRF", verbose = FALSE)
glmboost <- SimEval(data, ini = "random", method = "glmboost", verbose = FALSE)

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
     file = "results1.Rdata")
