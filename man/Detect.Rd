\name{Detect}
\alias{Detect}
\title{Detect variable type in a data matrix}
\usage{
Detect(x, n = 5, ...)
}
\arguments{
  \item{x}{is the data matrix that need to be detected.}

  \item{n}{is a number, indicating how many levels, if
  outnumbered, can be seen}

  \item{...}{other arguments as an numeric variable, rather
  than a categorical variable.}
}
\description{
This function detects the type of the variables in a data
matrix. Types can be continuous only, categorical only or
mixed type. The rule for defining a variable as a
categorical variable is when: (1) it is a character vector,
(2) it contains no more than n = 5 unique values
}

