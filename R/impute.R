#' Impute missing values by automated variable selection
#'
#' @param missdata data matrix with missing values encoded as NA.
#' @param lmFun the variable selection method for continuous data.
#' @param cFun the variable selection method for categorical data.
#' @param conv if is TRUE, then convergence status will be returned.
#' @param ini the method for initilisation, could be 
#' @param pred.type is the prediction type. It is no useful now
#' @param maxiter is the maximum number of interations
#' @param verbose is logical, if TRUE then detailed information will
#' be printed in the console while running.
#' @export
#' @return if conv = FALSE, then a completed data matrix, if TRUE, a list

impute <- function(missdata, lmFun = NULL, cFun = NULL, conv = TRUE, 
                   ini = "mean", pred.type = NULL, 
                   maxiter = 100, verbose = TRUE) {
  ## Detect variable types for the missing data and distribute appropriate tasks
  Type <- Detect(missdata)
  if(all(Type == "numeric")) {
    task <- 1
    names(task) <- "Regression"
  } else if(all(Type == "character")) {
    task <- 2
    names(task) <- 'Classification'
  } else {
    task <- 3
    names(task) <- "Regression and Classification mixed"
  }
  
  if (verbose) {
    cat(" We are doing task", names(task), "\n")
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
#   browser()
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
  
  ## perform initial guess on miss (mean imputation)
  ximp <- missdata
  if (task == 1) {
    ximp <- guess(ximp, type = ini)
  } else if (task == 2) {
    ximp <- guess(ximp, "majority")
  } else {
    # for mixed type only, code from 'missForest' source.
    xAttrib <- lapply(missdata, attributes)
    varType <- character(p)
    for (t.co in 1:p){
      if (is.null(xAttrib[[t.co]])) {
        varType[t.co] <- 'numeric'
        ximp[is.na(missdata[,t.co]),t.co] <- mean(missdata[,t.co], na.rm = TRUE)
        } else {
          varType[t.co] <- 'factor'
          ## take the level which is more 'likely' (majority vote)
          max.level <- max(table(ximp[,t.co]))
          ## if there are several classes which are major, sample one at random
          class.assign <- sample(names(which(max.level == summary(ximp[,t.co]))), 1)
          ## it shouldn't be the NA class
          if (class.assign != "NA's"){
            ximp[is.na(missdata[,t.co]),t.co] <- class.assign
            } else {
              while (class.assign == "NA's"){
                class.assign <- sample(names(which(max.level ==
                                                     summary(ximp[,t.co]))), 1)
              }
              ximp[is.na(missdata[,t.co]),t.co] <- class.assign
            }
        }
    }
  }
  
  
  ## extract missingness pattern
  NAloc <- is.na(missdata)            # where are missings
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  sort.j <- order(noNAvar) # indices of increasing amount of NA in vars
  sort.j <- rev(sort.j)
  sort.noNAvar <- noNAvar[sort.j]
  
  ## output
  Ximp <- vector('list', maxiter)
  
  if (conv) {
    Converg <- rep(NA, maxiter)
  }
  
  iter <- 0
  k <- length(unique(Type))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  
  ## setup convergence variables w.r.t. variable types
  if (k == 1) {
    if (unique(Type) == 'numeric'){
      names(convNew) <- c('numeric')
    } else {
      names(convNew) <- c('character')
    }
    convergence <- c()
  } else {
    names(convNew) <- c('numeric', 'character')
    convergence <- matrix(NA, ncol = 2)
  }
  
  ## function to yield the stopping criterion in the following 'while' loop
  stopCriterion <- function(Type, convNew, convOld, iter, maxiter){
    k <- length(unique(Type))
    if (k == 1){
      (convNew < convOld) & (iter < maxiter)
    } else {
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
    }
  }
  
  
  ## iterate 
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
        misX <- as.matrix(ximp[misi, seq(1, p)[-varInd]]) # prediction variables
        if (task == 1 && lmFun %in% 
             c("stepR", "ridgeR", "stepBothR", "stepBackR", "stepForR")) {
          # some class of predction functions require that the newdata be
          # a data.frame rather than a matrix, so we need to tranform the 
          # new data (matrix) into a data frame 
          misX <- as.data.frame(misX)
        } else if (task == 2 && cFun %in% 
                     c("stepBothC", "stepBackC", "stepForC", "rpartC",
                       "treeC", "gbmC", "ridgeC")) {
          # some class of predction functions require that the newdata be
          # a data.frame rather than a matrix, so we need to tranform the 
          # new data (matrix) into a data frame 
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
   
    ## check the difference between iteration steps
    t.co2 <- 1
        for (t.type in names(convNew)) {
            t.ind <- which(Type == t.type)
            if (t.type == "numeric") {
                convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, 
                  t.ind])^2)/sum(ximp[, t.ind]^2)
            } else {
                dist <- sum(as.character(as.matrix(ximp[, t.ind])) != 
                  as.character(as.matrix(ximp.old[, t.ind])))
                convNew[t.co2] <- dist/(n * sum(Type == "character"))
            }
            t.co2 <- t.co2 + 1
        }
    if (conv) {
      Converg[iter] <- convNew
    }
    if (verbose) {
      cat(" Conv diff after iteration", iter,  "is", convNew, "\n")
    }
  }
  #end while((convNew<convOld)&(iter<maxiter)){
  
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
