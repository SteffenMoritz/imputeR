#' Naive imputation for mixed type data
#' 
#' @param missdata a data matrix with missing values
#' @param method a character vector of length 2 indicating which two methods to use
#' respectively for continuous variables and categorical variables. There are three options
#' for continous variables: "mean", "median" and "random", and two options for categorical
#' varaibles: "majority" and "random". The default method is "mean" for the continous part
#' and "majority" for the categorical part.
#' @return the same size data matrix with no missing value.
#' 
#' 
#' @export
#' @examples
#' data(tic)
#' \dontrun{
#' missdata <- SimIm(tic, 0.1)
#' require(cutoffR)
#' nmissing(missdata)
#' HeatStruct(missdata)
#' impdata <- mixGuess(missdata)
#' nmissing(impdata)
#' }
mixGuess <- function(missdata, method = c("mean", "majority")) {
  Type <- Detect(missdata)
  ind1 <- which(Type == "numeric")
  ind2 <- which(Type == "character")
  newdata <- missdata
  newdata[, ind1] <- guess(newdata[, ind1], type = method[1])
  newdata[, ind2] <- guess(newdata[, ind2], type = method[2])
  return(newdata)
}