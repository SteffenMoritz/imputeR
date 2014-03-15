\name{impute}
\alias{impute}
\title{Impute missing values by automated variable selection}
\usage{
impute(missdata, lmFun = NULL, cFun = NULL, conv = TRUE, ini = "mean",
  pred.type = NULL, maxiter = 100, verbose = TRUE)
}
\arguments{
  \item{miss}{data matrix containing missing values}

  \item{FUN}{the imputation function to be applied}

  \item{conv}{if is TRUE, then convergence status will be
  returned.}

  \item{maxiter}{is the maximum number of interations}
}
\value{
if conv = FALSE, then a completed data matrix, if TRUE, a
list
}
\description{
Impute missing values by automated variable selection
}

