#' General Imputation Framework in R
#' 
#' @description Impute missing values under the general framework in R
#' @details This function can impute several kinds of data, including continuous-only
#' data, categorical-only data and mixed-type data. Many methods can be used, including
#' regularisation method like LASSO and ridge regression, tree-based model and dimensionality
#' reduction method like PCA and PLS.
#'
#' @param missdata data matrix with missing values encoded as NA.
#' @param lmFun the variable selection method for continuous data.
#' @param cFun the variable selection method for categorical data.
#' @param ini the method for initilisation. It is a length one character if
#' missdata contains only one type of variables only. For continous only data, 
#' ini can be "mean" (mean imputation), "median" (median imputation) or "random"
#'  (random guess), the default is "mean". For categorical data, it can be 
#'  either "majority" or "random", the default is "majority". If missdata is 
#'  mixed of continuous and categorical data, then ini has to be a vector of two
#'  characters, with the first element indicating the method for continous 
#'  variables and the other element for categorical variables, and the default
#'  is c("mean", "majority".)
#' @param maxiter is the maximum number of interations
#' @param verbose is logical, if TRUE then detailed information will
#' be printed in the console while running.
#' @param conv logical, if TRUE, the convergence details will be returned
#' 
#' @return if conv = FALSE, it returns a completed data matrix with no
#' missing values; if TRUE, it rrturns a list of components including:
#' 
#' \item{imp}{the imputed data matrix with no missing values}
#' \item{conv}{the convergence status during the imputation}
#' 
#' @seealso \code{\link{SimIm}} for missing value simulation.
#' 
#' @export
#' @examples
#' data(parkinson)
#' # introduce 10% random missing values into the parkinson data
#' missdata <- SimIm(parkinson, 0.1)
#' # impute the missing values by LASSO
#' \dontrun{
#' impdata <- impute(missdata, lmFun = "lassoR")
#' # calculate the normalised RMSE for the imputation
#' Rmse(impdata$imp, missdata, parkinson, norm = TRUE)
#' }
impute <- function(missdata, lmFun = NULL, cFun = NULL, ini = NULL, 
                   maxiter = 100, verbose = TRUE, conv = TRUE) {
  Type <- Detect(missdata)
  if(all(Type == "numeric")) {
    task <- 1
    if(is.null(ini)) {
      ini = "mean"
    } else {
      stopifnot(ini %in% c("mean", "median", "random"))
    }
    names(task) <- "Regression"
  } else if(all(Type == "character")) {
    task <- 2
    if(is.null(ini)) {
      ini = "majority"
    } else {
      stopifnot(ini %in% c("majority", "random"))
    }
    names(task) <- 'Classification'
  } else {
    task <- 3
    if(is.null(ini)) {
      ini = c("mean", "majority")
    } else {
      if (length(ini) !=2) {
        stop ("Data are mixed-type, provide two initial methods")
      } else {
        if (!(ini[1] %in% c("mean", "median", "random")) || 
              !(ini[2] %in% c("majority", "random"))) {
          stop ("ini has to be a two length character vector, check the help
                about this argument and make sure you provide valid names")
        }
      }
    }
    names(task) <- "Regression and Classification mixed"
  }
  if (verbose) {
    cat("Imputation task is:", names(task), "\n")
  }
  
  if (task == 1) {
    stopifnot(!is.null(lmFun))
    lmFUN <- match.fun(lmFun)
    cFUN <- NULL
  } else if (task == 2) {
    stopifnot(!is.null(cFun))
    lmFUN <- NULL
    cFUN <- match.fun(cFun)
  } else {
    stopifnot(!is.null(cFun), !is.null(lmFun))
    lmFUN <- match.fun(lmFun)
    cFUN <- match.fun(cFun)
  }
  n <- nrow(missdata)
  p <- ncol(missdata)
  
  ## remove completely missing variables
  if (any(apply(is.na(missdata), 2, sum) == n)) {
    ind <- which(apply(is.na(missdata), 2, sum) == n)
    missdata <- missdata[, -ind]
    p <- ncol(missdata)
    cat('removed variable(s)', ind,
        'due to the missingness of all entries\n')
  }
  ## perform initial guess on miss 
  ximp <- missdata
  if (task == 1) {
    ximp <- guess(ximp, type = ini)
  } else if (task == 2) {
    ximp <- guess(ximp, type = ini)
  } else {
    for (i in seq_along(Type)) {
      if (Type[i] == "numeric") {
        ximp[, i] <- guess(missdata[, i], type = ini[1])
        } else {
          if (ini[2] == "majority") {
            ximp[, i] <- as.numeric(major(missdata[, i]))
          } else {
            ximp[, i] <- guess(missdata[, i], type = "random")
          }
        }
    }
  }
  
  # extract missingness pattern
  NAloc <- is.na(missdata)
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  sort.j <- order(noNAvar, decreasing = TRUE) # indices of increasing amount of NA in vars
  sort.noNAvar <- noNAvar[sort.j]
  
  # ready for output
  Ximp <- vector('list', maxiter)
  
  iter <- 0
  k <- length(unique(Type))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  
  # setup convergence container w.r.t. task types
  if (k == 1) {
    if (unique(Type) == 'numeric'){
      names(convNew) <- c('numeric')
    } else {
      names(convNew) <- c('character')
    }
    Converg <- rep(NA, maxiter)
  } else {
    names(convNew) <- c('numeric', 'character')
    Converg <- matrix(NA, nrow = maxiter, ncol = 2)
  }
  
  # stopping function for the loop
  stopCriterion <- function(Type, convNew, convOld, iter, maxiter) {
    k <- length(unique(Type))
    if (k == 1) {
      (convNew < convOld) & (iter < maxiter)
    } else {
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
    }
  }

  while (stopCriterion(Type, convNew, convOld, iter, maxiter)) {
    if (iter != 0){
      convOld <- convNew     
    }
    
    if (verbose) {
      if (task == 1) {
        cat("iteration", iter + 1,  "using", lmFun, 
          "in progress...")
      } else if (task == 2) {
        cat("iteration", iter + 1,  "using", cFun, 
            "in progress...")
      } else {
        cat("iteration", iter + 1,  "using", lmFun, "and", cFun, 
            "in progress...")
      }
    }
    
    ximp.old <- ximp
    for (s in 1:p) {
      varInd <- sort.j[s]
      if (noNAvar[[varInd]] != 0) {
        obsi <- !NAloc[, varInd] # which i's are observed
        misi <- NAloc[, varInd] # which i's are missing
        obsY <- ximp[obsi, varInd] # training response
        obsX <- as.matrix(ximp[obsi, seq(1, p)[-varInd]]) # training variables
        colnames(obsX) <- paste0("x", 1:ncol(obsX))
        misX <- as.matrix(ximp[misi, seq(1, p)[-varInd]]) # predictors

        # as.df can be overwritten if some known functions that require data.frame
        # by their preidction function are called.
        if (task == 1 && (lmFun %in% 
             c("stepR", "ridgeR", "stepBothR", "stepBackR", "stepForR"))) {
          # some class of predction functions require that the newdata be
          # a data.frame rather than a matrix, so we need to tranform the 
          # new data (matrix) into a data frame 
          misX <- as.data.frame(misX)
        } else if (task == 2 && (cFun %in% 
                     c("stepBothC", "stepBackC", "stepForC", "rpartC",
                       "treeC", "gbmC", "ridgeC"))) {
          misX <- as.data.frame(misX)
        } else if (task == 3) {
          if ((lmFun %in% c("stepR", "ridgeR", "stepBothR", "stepBackR", "stepForR")) ||
                (cFun %in% c("stepBothC", "stepBackC", "stepForC", "rpartC",
                  "treeC", "gbmC", "ridgeC")))
            misX <- as.data.frame(misX)
        }
        
        colnames(misX) <- colnames(obsX)
        typeY <- Type[varInd]

        ## train model (with automated variable selction) on observed data
        if (typeY == "numeric") {
          Miss <- lmFUN(x = obsX, y = obsY)
          if (lmFun %in% c("pcrR", "plsR")) {
            misY <- predict(Miss, misX, ncomp = 2, type = "response")
          } else if (lmFun == "CubistR") {
            misY <- predict(Miss$model, misX, neighbors = Miss$neighbors)
          } else {
            misY <- predict(Miss, misX)
          }
        } else {
          obsY2 <- factor(obsY)
          summarY <- summary(obsY2)
          if (length(summarY) == 1) {
            # if all values of obsY is the same then using model would be 
            # unnecessary
            misY <- factor(rep(names(summarY), sum(misi)))
          } else {
            if (cFun %in% c("randomForest", "rdaC", "RRF")) {
              obsY <- factor(obsY)
            }
            Miss <- cFUN(x = obsX, y = obsY)
            if (cFun %in% c("stepBothC", "stepBackC", "stepForC", 
                            "lassoC", "ridgeC")) {
              misY <- ifelse(predict(Miss, misX, type = "response") < 0.5, 0, 1)
              } else if (cFun %in% c("rpartC", "randomForest", "RRF")) {
              try <- predict(Miss, misX, type = "prob")
              misY <- ifelse(try[, 1] > try[, 2], 0, 1)
              } else if (cFun == "treeC") {
                misY <- predict(Miss, misX, type = "class")
              } else if (cFun == "rdaC") {
                try <- predict(Miss$fit, Miss$x, Miss$y, xnew = t(misX), alpha = Miss$alpha,
                              delta = Miss$delta, type = "posterior")
                misY <- ifelse(try[, 1] > try[, 2], 0, 1)
              } else if (cFun == "gbmC") {
                try <- predict(Miss$model, misX, Miss$best, type = "response")
                misY <- ifelse(try < 0.5, 0, 1)
              } else {
              misY <- predict(Miss, misX)
              }
          }
        }  
        ## replace old imputed value with prediction
        ximp[misi, varInd] <- misY
      }
    }  
    if (verbose) {
      cat('done!\n')
    }
    iter <- iter + 1
    Ximp[[iter]] <- ximp
    # check the difference between iteration steps
    # This implementation is really smart and is derived from the brillian MissForest
    # package source.
    t.co2 <- 1
        for (t.type in names(convNew)) {
            t.ind <- which(Type == t.type)
            if (t.type == "numeric") {
                convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, t.ind])^2)/sum(ximp[, t.ind]^2)
            } else {
                dist <- sum(as.character(as.matrix(ximp[, t.ind])) != 
                  as.character(as.matrix(ximp.old[, t.ind])))
                convNew[t.co2] <- dist/(n * sum(Type == "character"))
            }
            t.co2 <- t.co2 + 1
        }
    if (conv) {
      if (k == 1) {
        Converg[iter] <- convNew
      } else {
        Converg[iter, ] <- convNew
      }
    }
    if (verbose) {
      if (task == 3) {
        cat("Difference after iteration", iter,  "is", convNew[1],
            "and", convNew[2], "\n") 
      } else {
        cat("Difference after iteration", iter,  "is", convNew, "\n")
      }
    }
  }
  
  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    ximp = Ximp[[iter]]
  } else {    
    ximp = Ximp[[iter - 1]]
  }
  if (conv) {
    Converg <- na.omit(Converg)
    return(list(imp = ximp, conv = Converg))
  }
  return(ximp)
}