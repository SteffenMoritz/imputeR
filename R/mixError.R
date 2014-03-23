#' Calculate mixed error when the imputed matrix is mixed type
#' 
#' @param imp the imputed matrix
#' @param mis the original matrix with missing values
#' @param true the true matrix
#' @param norm logical, if TRUE, the nomailised RMSE will return for continous variables
#' @export
mixError <- function(imp, mis, true, norm = TRUE) {
  Type <- Detect(imp)
  err <- rep(NA, 2)
  if (norm) {
    names(err) <- c('NRMSE', 'MCE')
  } else {
    names(err) <- c('RMSE', 'MCE')
  }
  for (t.type in Type) {
    t.ind <- which(Type == t.type)
    if (t.type == "numeric"){
      err[1] <- Rmse(imp[,t.ind], mis[,t.ind], true[,t.ind], norm = norm)
    } else {
      err[2] <- mr(imp[,t.ind], mis[,t.ind], true[,t.ind])
    }
  }
  return(err)
}