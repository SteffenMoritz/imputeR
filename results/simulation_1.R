library(impute)
# read the parkinson data and manipulate 
parkinson <- read.csv("./data/parkinson.csv")
# delete the first and 18th column, they are not numeric predictors
data <- scale(parkinson[, -c(1, 18)])
# introduce 10% missing values, 100 simulations
n.sim = 100
p = 0.1
# Use mean imputation
r_base <- SimEval(data, n.sim = n.sim, guess = TRUE, guess.type = "mean")

# all lmFuns

lmfuns <- c("stepBackR", "stepForR", "stepBothR", "lassoR", "pcrR", "plsR",
           "randomForest", "earth", "glmboost", "ridgeR")

system.time(
  for (i in seq_along(lmfuns)) {
    assign(paste("sim", lmfuns[i], sep = "_"),  SimEval(data, p = p, n.sim = n.sim, method = lmfuns[i], verbose = FALSE))
  }
)

# derive all the errors
sim_error <- sapply(paste("sim", lmfuns, sep = "_"), function(i) eval(as.name(i))$error)

# combine to base
sim_error <- cbind(r_base$error, sim_error)
colnames(sim_error) <- c("mean", lmfuns)
require(reshape2)
sim_error2 <- melt(sim_error)[, 2:3]
names(sim_error2) <- c("method", "nrmse")
oind <- order(as.numeric(by(sim_error2$nrmse, sim_error2$method, mean)), decreasing = TRUE)
sim_error2$method <- ordered(sim_error2$method , levels = levels(sim_error2$method)[oind])  
# boxplot
par(las = 2)
par(mar = c(7, 4.1, 4.1, 2))
boxplot(nrmse ~ method, data = sim_error2, notch = T, col = "bisque", ylab = "NRMSE")
means <- colMeans(sim_error)[oind]
points(means, col = 2)
# computational time
sim_time <- sapply(paste("sim", lmfuns, sep = "_"), function(i) eval(as.name(i))$time)
sim_time <- cbind(base$time, sim_error)

# convergency iterations
sim_conv


mean1 <- apply(nrmse1, 2, mean)
sd1 <- apply(nrmse1, 2, sd)
time1 <- s
pvalue <- numeric(length(mean1))
for (i in seq_along(pvalue)) {
  pvalue[i] <- wilcox.test(nrmse1[, i], nrmse1[, 1], alternative = "less")$p.value
}
results <- cbind(mean1, sd1, time1, conv1)
rownames(results) <- colnames(nrmse1)
table1 <- xtable(results)
digits(table1)[c(2:4)] <- 4
print(table1)
save(base, lasso, rf, ridge, stepback, stepBoth, stepfor, earth, rrf, glmboost, 
     file = "results1.Rdata")
