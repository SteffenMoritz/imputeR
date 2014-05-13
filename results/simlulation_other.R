
# Simulation by some other methods ----------------------------------------
simdata <- SimIm(data, 0.3)
base <- SimEval(data, p = 0.3, guess = TRUE)
mice <- SimEval(data, p = 0.3, other = "missMICE", verbose = FALSE)
knn <- SimEval(data, p = 0.3, other = "missKNN", verbose = FALSE)
svd <- SimEval(data, p = 0.3, other = "missSVD", verbose = FALSE)

mean(knn$time)
mean(svd$time)
other_re <- cbind(knn$nrmse, svd$nrmse)
apply(other_re, 2, mean)
apply(other_re, 2, sd)
