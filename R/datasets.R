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


#' SPECT Heart Data Set 
#' 
#' The dataset describes diagnosing of cardiac Single Proton Emission Computed Tomography (SPECT) images
#' Each of the patients is classified into two categories: normal and abnormal. 
#' The database of 267 SPECT image sets (patients) was processed to extract features 
#' that summarize the original SPECT images. As a result, 44 continuous feature pattern
#' was created for each patient. The pattern was further processed to obtain 22 binary feature patterns. 
#' The CLIP3 algorithm was used to generate classification rules from these patterns. 
#' The CLIP3 algorithm generated rules that were 84.0% accurate (as compared with cardilogists' diagnoses). 
#' SPECT is a good data set for testing ML algorithms; it has 267 instances that are descibed by 23 binary attributes.
#' In the imputation study, it can be treated as a categorical-only data. For detailed information, please refer to
#' the Source and the Reference
#' 
#' \itemize{
#'  \item X1. OVERALL_DIAGNOSIS: 0,1 (class attribute, binary) 
#'  \item X0. F1: 0,1 (the partial diagnosis 1, binary) 
#'  \item ...
#'  }
#' @source \url{http://archive.ics.uci.edu/ml/datasets/SPECT+Heart}
#' @references Kurgan, L.A., Cios, K.J., Tadeusiewicz, R., Ogiela, M. & Goodenday, L.S. 2001
#' Knowledge Discovery Approach to Automated Cardiac SPECT Diagnosis 
#' \emph{Artificial Intelligence in Medicine}, vol. 23:2, pp 149-169
#' @docType data
#' @keywords datasets
#' @format A data frame with 266 rows and 23 variables
#' @name spect
NULL

#' Insurance Company Benchmark (COIL 2000) Data Set 
#' 
#' This data set used in the CoIL 2000 Challenge contains information on customers of an insurance company. 
#' The data consists of 86 variables and includes product usage data and socio-demographic data. Detailed
#' information, please refer to the Source. For imputation study, this dataset can be treated as a mixed-type
#' data.
#' 
#' \itemize{
#'  \item V1. a numeric variable
#'  \item V2. a categorical variable
#'  \item ...
#'  }
#' 
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Insurance+Company+Benchmark+(COIL+2000)}
#' @references P. van der Putten and M. van Someren (eds). CoIL Challenge 2000: 
#' The Insurance Company Case. Published by Sentient Machine Research, Amsterdam. 
#' Also a Leiden Institute of Advanced Computer Science Technical Report 2000-09. June 22, 2000. 
#' @docType data
#' @keywords datasets
#' @format A data frame with 266 rows and 23 variables
#' @name tic
NULL

