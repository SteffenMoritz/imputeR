
# Convergence Study -------------------------------------------------------

# subset methods, ini = mean
simdata <- SimIm(data, 0.1)
conv01 <- vector("list", length(imp_methods))
for (i in seq_along(conv01)) {
  conv01[[i]] <- impute(simdata, FUN = imp_methods[i])$conv
}

conv02 <- vector("list", length(imp_methods))
for (i in seq_along(conv02)) {
  conv02[[i]] <- impute(simdata, ini = "random", FUN = imp_methods[i])$conv
}


# Plot the convergence ----------------------------------------------------
par(mfrow = c(2, 2))

# Row1 --------------------------------------------------------------------
plot(conv01[[1]], xlim = c(1, 6), type = "b", col = 1, lty = 1, lwd = 2, xlab = "Iterations", 
     ylab = "Iteration Difference")
for(i in 2:4) {
  lines(conv01[[i]], type = "b", lty = i, col = i, lwd = 2)
}
legend("topright", names_methods[1:4], lty = 1:6, col = 1:6)

plot(conv02[[1]], xlim = c(1, 10), type = "b", col = 1, lty = 1, lwd = 2, xlab = "Iterations", 
     ylab = "Iteration Difference")
for(i in 2:4) {
  lines(conv02[[i]], type = "b", lty = i, col = i, lwd = 2)
}
legend("topright", names_methods[1:4], lty = 1:6, col = 1:6)


# Row2 --------------------------------------------------------------------


plot(conv01[[5]], ylim = c(0, 0.01), xlim = c(1, 6),
     type = "b", col = 1, lty = 1, lwd = 2, xlab = "Iterations", 
     ylab = "Iteration Difference")
for(i in 6:8) {
  lines(conv01[[i]], type = "b", lty = i - 4, col = i - 4, lwd = 2)
}
legend("topright", names_methods[5:8], lty = 1:4, col = 1:4)

plot(conv02[[5]], ylim = c(0, 0.013), xlim = c(1, 6),
     type = "b", col = 1, lty = 1, lwd = 2, xlab = "Iterations", 
     ylab = "Iteration Difference")
for(i in 6:8) {
  lines(conv02[[i]], type = "b", lty = i - 4, col = i - 4, lwd = 2)
}
legend("topright", names_methods[5:8], lty = 1:4, col = 1:4)
par(mfrow = c(1, 1))

# Row3 --------------------------------------------------------------------
plot(conv01[[9]], type = "b", col = 1, lty = 1, lwd = 2, xlab = "Iterations", 
     ylab = "Iteration Difference")
legend("topright", names_methods[9])
plot(conv02[[9]], type = "b", col = 1, lty = 1, lwd = 2, xlab = "Iterations", 
     ylab = "Iteration Difference")
legend("topright", names_methods[9])
par(mfrow = c(1, 1))