library(impute)
require(doMC, foreach)

registerDoMC(cores = 6)
# how many workers for 
getDoParWorkers()

# read the parkinson data and manipulate 
spect <- read.csv("./data/spect.csv")
data <- spect
Detect(data)
# introduce 10% missing values, 100 simulations
n.sim = 100
p = 0.1
# Use mean imputation
c_base <- SimEval(data, n.sim = n.sim, guess = TRUE, guess.type = "majority")
c_base
# all lmFuns

cfuns <- c("stepBackC", "stepForC", "stepBothC", "lassoC", "rpartC", "rdaC",
            "randomForest", "gbmC", "ridgeC")

system.time(
  foreach (i = seq_along(cfuns)) %dopar% 
    (assign(paste("csim", cfuns[i], sep = "_"),  SimEval(data, p = p, n.sim = n.sim, method = cfuns[i], verbose = FALSE))
     )
)

# derive all the errors
csim_error <- sapply(paste("csim", cfuns, sep = "_"), function(i) eval(as.name(i))$error)

# combine to base
csim_error <- cbind(c_base$error, csim_error)
colnames(csim_error) <- c("mean", cfuns)
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