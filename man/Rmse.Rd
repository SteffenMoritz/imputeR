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

  \item{norm}{logical, if TRUE then the normalized RMSE
  will be returned}
}
\description{
This function calculate imputation error given the imputed
data, the missing data and the true data
}

