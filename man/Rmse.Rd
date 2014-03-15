\name{Rmse}
\alias{Rmse}
\title{calculate the RMSE or NRMSE}
\usage{
Rmse(imp, mis, true, norm = FALSE)
}
\arguments{
  \item{imp}{the imputaed data matrix}

  \item{mis}{the missing data matrix}

  \item{true}{the true data matrix}

  \item{logical,}{if TRUE then the normalized RMSE will be
  returned}
}
\description{
This function calculate imputation error given the imputed
data, the missing data and the true data
}
\examples{
set.seed(1234)
data <- rnorm(100)
misdata <- SimIm(data, p = 0.1)
impdata <- guess(data)
rmse <- Rmse(impdata, misdata, data)
}

