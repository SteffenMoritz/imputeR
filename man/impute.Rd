\name{impute}
\alias{impute}
\title{Impute missing values by automated variable selection}
\usage{
impute(missdata, lmFun = NULL, cFun = NULL, conv = TRUE, ini = "mean",
  pred.type = NULL, maxiter = 100, verbose = TRUE)
}
\arguments{
  \item{missdata}{data matrix with missing values encoded
  as NA.}

  \item{lmFun}{the variable selection method for continuous
  data.}

  \item{cFun}{the variable selection method for categorical
  data.}

  \item{conv}{if is TRUE, then convergence status will be
  returned.}

  \item{ini}{the method for initilisation, could be}

  \item{pred.type}{is the prediction type. It is no useful
  now}

  \item{maxiter}{is the maximum number of interations}

  \item{verbose}{is logical, if TRUE then detailed
  information will be printed in the console while
  running.}
}
\value{
if conv = FALSE, then a completed data matrix, if TRUE, a
list
}
\description{
Impute missing values by automated variable selection
}

