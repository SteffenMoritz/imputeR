#' Parkinsons Data Set
#' 
#' This dataset contains a range of biomedical voice measurements from 31 people, 23 with 
#' Parkinson's disease. Each row corresponds to one of 195 individuals and each column a
#' measurement variable. This data was originally obtaind from the UCI Machine Learning Repository.
#' For detailed information about the columns, see the reference and the source below. 
#' In the study of simulation, this dataset can be treated as continuous-only data
#' 
#' \itemize{
#'   \item MDVP:Fo(Hz). Average vocal fundamental frequency  
#'   \item MDVP:Fhi(Hz). Maximum vocal fundamental frequency 
#'   \item MDVP:Flo(Hz). Minimum vocal fundamental frequency 
#'   \item ...
#' }
#' 
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Parkinsons}
#' @references Little MA, McSharry PE, Roberts SJ, Costello DAE, Moroz IM, 2007 
#' Exploiting Nonlinear Recurrence and Fractal Scaling Properties for Voice Disorder Detection,
#' \emph{BioMedical Engineering OnLine}
#' @docType data
#' @keywords datasets
#' @format A data frame with 195 rows and 22 variables
#' @name parkinson
NULL