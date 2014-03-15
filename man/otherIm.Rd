\name{otherIm}
\alias{otherIm}
\title{Impute missing data matrix by some other imputation methods}
\usage{
otherIm(method = NULL, misdata = simdata, truedata = data, ...)
}
\arguments{
  \item{method}{is the imputation method, possibly from
  other packages}

  \item{missdata}{is the missing data need to be imputed}

  \item{truedata}{is the true data matrix}

  \item{...}{some other arguments that can be passed to
  method}
}
\description{
This function uses some other imputation methods that are
not introduced in our package.
}

