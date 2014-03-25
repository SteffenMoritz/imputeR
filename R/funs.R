#' Introduce some missing values into a data matrix
#' 
#' This function randomly introduce some amount of missing values into a matrix
#' 
#' @param data a data matrix to simulate
#' @param p the percentage of missing values introduced into the data matrix
#'    it should be a value between 0 and 1.
#' @keywords simulation, imputation
#' @export
#' @examples
#' simdata <- matrix(rnorm(100), 10, 10)
#' missingdata <- SimIm(simdata, p = 0.15)
#' # count the number of missing values afterwards
#' sum(is.na(missingdata))

SimIm <- function(data, p = 0.1) {
  vec <- c(unlist(data))
  missing <- rbinom(length(vec), 1, p)
  vec[missing == 1] <- NA
  dim(vec) <- dim(data)
  return(vec)
}

#' calculate the RMSE or NRMSE
#' 
#' This function calculate imputation error given the imputed data, the missing
#' data and the true data
#' 
#' @param imp the imputaed data matrix
#' @param mis the missing data matrix
#' @param true the true data matrix
#' @param norm logical, if TRUE then the normalized RMSE will be returned
#' 
#' @export
Rmse <- function(imp, mis, true, norm = FALSE) {
  # if norm is true, calculate the normalised RMSE
  # coerce both matrices into vectors
  
  imp <- as.matrix(imp)
  mis <- as.matrix(mis)
  true <- as.matrix(true)
  
  missIndex <- which(is.na(mis))
  errvec <- imp[missIndex] - true[missIndex]
  rmse <- sqrt(mean(errvec^2))
  if (norm) {
    rmse <- rmse/sd(true[missIndex])
  }
  return(rmse)
}


#' calculate miss-classification error 
#' 
#' This function calculates the misclassfication error given the imputed data, 
#' the missing data and the true data.
#' @param imp the imputaed data matrix
#' @param mis the missing data matrix
#' @param true the ture data matrix
#' @export 
mr <- function(imp, mis, true) {
  
  # if norm is true, calculate the normalised RMSE
  # coerce both matrices into vectors
  
  imp <- as.matrix(imp)
  mis <- as.matrix(mis)
  true <- as.matrix(true)
  
  missIndex <- which(is.na(mis))
  errvec <- table(as.numeric(imp[missIndex]), as.matrix(true)[missIndex])
  mr <- 1 - (sum(diag(errvec))/sum(errvec))
  return(mr)
}

#' Plot function for imputation
#' 
#' this is a plot function for assessing imputation performance given the imputed data
#' and the original true data
#' @param imp the imputed data matrix
#' @param mis the missing data matrix
#' @param true, the true data matrix
#' @param ... other arguments that can be passed to plot
#' @export
plotIm <- function(imp, mis, true, ...) {
  imp <- as.matrix(imp)
  mis <- as.matrix(mis)
  true <- as.matrix(true)
  
  missIndex <- which(is.na(mis))
  plot(imp[missIndex], true[missIndex], xlab = "Imputed Value", pch = 19, 
       cex = 0.8, ylab = "True Value", main = "Imputation Performance", ...)
  abline(0, 1, col = 2, lwd = 1.2, lty = 2)
  
}

#' Impute missing data matrix by some other imputation methods
#' 
#' This function uses some other imputation methods that are not introduced in
#' our package. 
#' @param method is the imputation method, possibly from other packages
#' @param misdata is the missing data need to be imputed
#' @param truedata is the true data matrix
#' @param ... some other arguments that can be passed to method
#' 
#' @export
otherIm <- function(method = NULL, misdata = simdata, truedata = data, ...) {
  
  FUN <- match.fun(method)
  if (identical(FUN, SVDmiss)) {
    impdata<- FUN(misdata, ...)$Xfill
  }
  else {
    impdata<- FUN(misdata, ...)$x
  }
  rmse <- Rmse(impdata, misdata, truedata, norm = TRUE)
  plotIm(impdata, misdata, data)
  return(rmse)
}

# 
# ma = function(i) {
#   i[is.na(i)] <- mean(i, na.rm = TRUE)
#   return(i)
# }


#' Evaluate the performance of a imputation method by simulation
#' 
#' @param data is the complete data matrix that will be used for simulation
#' @param task task type, either be 1 for regression, 2 for classification or 3 for
#' mixed type
#' @param p is the percentage of missing values that will be introduction into
#'   data, it has to be a value between 0 and 1
#' @param n.sim the number of simulations, default is 100 times
#' @param ini is the initialization setting for some relevant imputation methods
#'   , the default setting is "mean", while "median" and "random" can also be 
#'   used. See also \code{\link{guess}}
#' @param method the imputaion method based on variable selection for simulation
#'   some other imputation method can be passed to the 'other' argument
#' @param guess logical value, if is TRUE, then \code{\link{guess}} will be used
#'   as the imputation method for simulation
#' @param guess.method, guess type for the guess function. It cannot be NULL if guess is TRUE
#' @param other some other imputation method that is based on variable selection
#'   can be used. The requirement for this 'other' method is strict: it receives
#'   a data matrix including missing values and returns a complete data matrix.
#' @param verbose logical, if TRUE, additional output information will be provided 
#'   during iterations, i.e., the method that is using, the iteration number,
#'   the convegence difference as compared to the precious iteration. The 
#'   progression bar will show up irrespective of this option and it can not be
#'   got rid of. 
#'   @param seed set the seed for simulation so simulations using different imputation
#'   methods are comparable. The default value is set to 1234, which is not supposed to 
#'   mean anything. But if 1234 is used, then the seed for simulating the first
#'   missing data matrix is 1234, then it sums by one for every subsequent
#'   simulationg data matrix. 
#' @export
SimEval <- function(data, task = NULL, p = 0.1, n.sim = 100, ini = "mean", 
                    method = NULL, guess = FALSE, guess.method = NULL, 
                    other = NULL, verbose = TRUE, seed = 1234) {
  if (!guess & is.null(method) & is.null(other)) {
    stop("Please provide a method to impute or you can guess by setting 'guess = TRUE'")
  }
  if (!is.null(other)) {
    otherFun <- match.fun(other)
  }
  Type <- Detect(data)
  if (is.null(task)) {
    if (all(Type == "numeric")) {
      task <- 1
      if (is.null(guess.method)) {
        guess.method = "mean"
      }
      Fun1 <- method
      names(task) <- "Regression"
    } else if (all(Type == "character")) {
      task <- 2
      if (is.null(guess.method)) {
        guess.method = "majority"
      }
      Fun2 <- method
      names(task) <- "Classification"
    } else {
      task <- 3
      if(is.null(guess.method)) {
        guess.method = c("mean", "majority")
      }
      Fun1 <- method[1]
      Fun2 <- method[2]
      names(task) <- "Regression and Classification mixed"
    }
  } else {
    if (task == 1) {
      if (is.null(guess.method)) {
        guess.method = "mean"
      }
      names(task) <- "Regression"
      Fun1 <- method
    } else if (task == 2) {
      Fun2 <- method
      if (is.null(guess.method)) {
        guess.method = "majority"
      }
      names(task) <- "Classification"
    } else {
      if (is.null(guess.method)) {
        guess.method = c("mean", "majority")
      }
      Fun1 <- method[1]
      Fun2 <- method[2]
      names(task) <- "Regression and Classification mixed"
    }
  }
  time <- numeric(n.sim)
  if (task == 3) {
    error <- matrix(NA, nrow = n.sim, ncol = 2)
  } else {
    error <- numeric(n.sim)
  }
  if (!guess & is.null(other)) {
    conv <- vector("list", n.sim)
  }
  pb <- txtProgressBar(min = 0, max = n.sim, style = 3)
  for (i in seq_len(n.sim)) {
    setTxtProgressBar(pb, i)
    set.seed(seed + i)
    simdata <- SimIm(data, p = p)
    if (!guess & is.null(other)) {
      if (task == 1) {
        time[i] <- system.time(imp <- impute(simdata, ini = ini, lmFun = Fun1, 
                                             verbose = verbose))[3] 
        error[i] <- Rmse(imp$imp, simdata, data, norm = TRUE)
        conv[[i]] <- length(imp$conv)
      } else if (task == 2) {
        time[i] <- system.time(imp <- impute(simdata, cFun = Fun2, 
                                             verbose = verbose))[3] 
        error[i] <- mr(imp$imp, simdata, data)
        conv[[i]] <- length(imp$conv)
      } else {
        time[i] <- system.time(imp <- impute(simdata, lmFun = Fun1, cFun = Fun2, 
                                             verbose = verbose))[3] 
        error[i, ] <- mixError(imp$imp, simdata, data)
        conv[[i]] <- length(imp$conv)
      }
    } else if (!is.null(other)) {
      time[i] <- system.time(imp <- otherFun(simdata))[3]
      error[i] <- Rmse(imp, simdata, data, norm = TRUE)
    } else {
      stopifnot(!is.null(guess.method))
      if (task == 1) {
        time[i] <- system.time(imp <- guess(simdata, type = guess.method))[3]
        error[i] <- Rmse(imp, simdata, data, norm = TRUE)
      } else if (task == 2) {
        time[i] <- system.time(imp <- guess(simdata, type = guess.method))[3]
        error[i] <- mr(imp, simdata, data)
      } else {
        time[i] <- system.time(try1 <- mixGuess(simdata, method = guess.method))[3]
        error[i, ] <- mixError(try1, simdata, data)
      }
    }
    close(pb)
  }
  if (guess | !is.null(other)) {
    conv <- "No convergence"
  }
  return(list(call = as.character(substitute(method)), task = task, time = mean(time),
              error = error, conv = conv))
}

#' Detect variable type in a data matrix
#' 
#' This function detects the type of the variables in a data matrix. Types 
#' can be continuous only, categorical only or mixed type. The rule for
#' defining a variable as a categorical variable is when: (1) it is a character
#' vector, (2) it contains no more than n = 5 unique values
#' @param x is the data matrix that need to be detected. 
#' @param n is a number, indicating how many levels, if outnumbered, can be seen
#' as an numeric variable, rather than a categorical variable. 
#' @export
Detect <- function(x, n = 5) {
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x has to be either a matrix or data frame")
  }
  x <- as.data.frame(x)
  is.n <- function(i) {
    (is.numeric(i) || is.integer(i) || is.double(i))
  }
  
  f.new <- function(z) {
    if (is.factor(z)) {
      return ("character")
    } else if (is.n(z) && length(unique(unclass(z))) > n) {
      return("numeric")
    } else {
      return("character")
    }
  }
  p <- sapply(x, f.new)
  return (varType = p)
} 


#' Impute by guessing
#' 
#' This function use some primitive methods, including mean imputation, 
#' median imputation, random guess, or majority imputation (only for categorical
#' variables), to impute a missing data matrix.
#' @param x a matrix or data frame
#' @param type is the guessing type, including "mean" for mean imputation, 
#' "median" for median imputation, "random" for random guess, and "majority" for
#' majority imputation for categorical variables. 
#' @export
guess <- function(x, type = "mean") {
  # this function impute a missing data matrix by some guess
  # type can be "mean", "median", "random", or "majority" (only for discrete
  switch(type,
         mean = sapply(as.data.frame(x), FUN = function(i) {
           i[is.na(i)] <- mean(i, na.rm = TRUE)
           return(i)
         }),
         median = sapply(as.data.frame(x), FUN = function(i) {
           i[is.na(i)] <- median(i, na.rm = TRUE)
           return(i)
         }),
         random = sapply(as.data.frame(x), FUN = function(i) {
           i[is.na(i)] <- sample(na.omit(i), 1)
           return(i)
         }),
         majority = apply(sapply(as.data.frame(x), FUN = major), 2, as.numeric)
         
  )
}

#' Majority imputation for a vector
#' 
#' @param x a character (or numeric categorical) vector with missing values
#' @export
major <- function(x) {
  max.level <- max(table(as.factor(x)))
  ## if there are several classes which are major, sample one at random
  class.assign <- sample(names(which(max.level == summary(na.omit(as.factor(x))))), 1)
  x[is.na(x)] <- class.assign
  return(x)
}

#' Ordered boxplot for a data matrix
#' 
#' @param x a matrix
#' @param names a length two character vector, default is c("method, "MCE")
#' @param order.by which statistics to order by, default is mean
#' @param decreasing default is TRUE, the boxplot will be arranged in a decreasing order
#' @param notch logical, default is TRUE
#' @param col color for the boxplots, default is "bisque". 
#' @param mar the margin for the plot, adjust it to your need.
#' @param ... some other arguments that can be passed to the boxplot function
#' @export
orderbox <- function(x, names = c("method", "MCE"), order.by = mean, 
                     decreasing = TRUE, notch = TRUE, col = "bisque", 
                     mar = c(7, 4.1, 4.1, 2), ...) {
  x2 <- melt(x)[, 2:3]
  names(x2) <- names
  oind <- order(as.numeric(by(x2[, 2], x2[, 1], order.by)), decreasing = decreasing)
  x2[, names[1]]<- ordered(x2[, names[1]] , levels = levels(x2[, names[1]])[oind])  
  par(las = 2)
  par(mar = mar)
  boxplot(as.formula(paste(names[2], "~", names[1])), data = x2, notch = notch, 
          col = col, ylab = names[2], ...)
}