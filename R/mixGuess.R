#' Naive imputation for mixed type data
#' 
#' @param missdata a data matrix with missing values
#' @param method a character vector of length 2 indicating which two methods to use
#' respectively for continuous variables and categorical variables. There are three options
#' for continous variables: "mean", "median" and "random", and two options for categorical
#' varaibles: "majority" and "random". The default method is "mean" for the continous part
#'and "majority" for the categorical part.
#' @export
mixGuess <- function(missdata, method = c("mean", "majority")) {
  Type <- Detect(imp)
  ind1 <- which(Type == "numeric")
  ind2 <- which(Type == "character")
  
    if (t.type == "numeric"){
      missdata[, t.type] <- Rmse(imp[,t.ind], mis[,t.ind], true[,t.ind], norm = norm)
    } else {
      err[2] <- mr(imp[,t.ind], mis[,t.ind], true[,t.ind])
    }
  }
  return(err)
}