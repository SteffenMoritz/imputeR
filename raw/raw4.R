p = 0.3
n.sim = 100
base3_3 <- SimEval(data, n.sim = n.sim, p = p, guess = TRUE)
try3_3 <- SimEval(data, n.sim = n.sim, p = p, method = c(lmfuns[8], cfuns[5]))
colMeans(try3$error)
colMeans(base3$error)

apply(try3$error, 2, sd)
apply(base3$error, 2, sd)
wilcox.test(base3$error[, 1], try3$error[, 1])
wilcox.test(base3$error[, 2], try3$error[, 2])

save(base3, base3_3, try3, try3_3, file = "simulation_mixed.Rdata")

colMeans(try3_3$error)
colMeans(base3_3$error)


apply(try3_3$error, 2, sd)
apply(base3_3$error, 2, sd)
wilcox.test(base3_3$error[, 1], try3_3$error[, 1])
wilcox.test(base3_3$error[, 2], try3_3$error[, 2])
