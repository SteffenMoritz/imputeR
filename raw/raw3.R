library(impute)
require(MASS)
# Parkinson Data ----------------------------------------------------------
data <- Boston
# house <- read.csv("./data/housing.csv")

Detect(data)
simdata <- SimIm(data, 0.3)
# delete the first and 18th column, they are not numeric predictors
data <- scale(parkinson[, -c(1, 18)])
Detect(data)
set.seed(1234)
simdata <- SimIm(data, 0.1)