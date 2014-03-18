## Try on a classification problem


source("./code/methods.R")
source("./code/methods2.R")
source("./code/funs.R")
source("./code/impute.R")
# library(cutoff)
require(mvtsplot)
# Parkinson Data ----------------------------------------------------------
spect <- read.csv("./data/spect.csv")
str(spect)
Detect(spect)

data <- spect
set.seed(1234)
simdata <- SimIm(data, 0.1)
sum(is.na(simdata))
mvtsplot(simdata)

# Baseline, use majority guess

baseline <- guess(simdata, type = "majority")
(mr_base <- mr(baseline, simdata, data))

# Use step with both direction
system.time(cstep1 <- impute(simdata, cFun = "stepForC"))
(mr_step1 <- mr(cstep1$imp, simdata, data))

system.time(cstep2 <- impute(simdata, cFun = "stepBackC"))
(mr_step1 <- mr(cstep2$imp, simdata, data))

system.time(cstep3 <- impute(simdata, cFun = "stepBothC"))
(mr_step3 <- mr(cstep3$imp, simdata, data))

# rpart
system.time(crpart <- impute(simdata, cFun = "rpartC"))
(mr_rpart <- mr(crpart$imp, simdata, data))



# #treeC
# system.time(ctree <- impute(simdata, cFun = "treeC"))
# (mr_rpart <- mr(ctree$imp, simdata, data))

# rda
system.time(crda <- impute(simdata, cFun = "rdaC"))
(mr_rda <- mr(crda$imp, simdata, data))

# lasso
system.time(classo <- impute(simdata, cFun = "lassoC"))
(mr_lasso <- mr(classo$imp, simdata, data))

# ridge logistic

system.time(cridge <- impute(simdata, cFun = "ridgeC"))
(mr_ridge <- mr(cridge$imp, simdata, data))

# randomforest
system.time(crf <- impute(simdata, cFun = "randomForest"))
(mr_rf <- mr(crf$imp, simdata, data))

# rrf

system.time(crrf <- impute(simdata, cFun = "RRF"))
(mr_rrf <- mr(crrf$imp, simdata, data))

# gbm
system.time(cgbm <- impute(simdata, cFun = "gbmC"))
(mr_gbm <- mr(cgbm$imp, simdata, data))
