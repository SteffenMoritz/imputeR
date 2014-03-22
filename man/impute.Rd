\name{impute}
\alias{impute}
\title{Impute missing values by automated variable selection}
\usage{
impute(missdata, lmFun = NULL, cFun = NULL, ini = NULL, maxiter = 100,
  verbose = TRUE, conv = TRUE)
}
\arguments{
  \item{missdata}{data matrix with missing values encoded
  as NA.}

  \item{lmFun}{the variable selection method for continuous
  data.}

  \item{cFun}{the variable selection method for categorical
  data.}

  \item{ini}{the method for initilisation. It is a length
  one character if missdata contains only one type of
  variables only. For continous only data, ini can be
  "mean" (mean imputation), "median" (median imputation) or
  "random" (random guess), the default is "mean". For
  categorical data, it can be either "majority" or
  "random", the default is "majority". If missdata is mixed
  of continuous and categorical data, then ini has to be a
  vector of two characters, with the first element
  indicating the method for continous variables and the
  other element for categorical variables, and the default
  is c("mean", "majority".)}

  \item{maxiter}{is the maximum number of interations}

  \item{verbose}{is logical, if TRUE then detailed
  information will be printed in the console while
  running.}

  \item{conv}{logical, if TRUE, the convergence details
  will be returned}
}
\value{
if conv = FALSE, then a completed data matrix, if TRUE, a
list
}
\description{
Impute missing values by automated variable selection
}

