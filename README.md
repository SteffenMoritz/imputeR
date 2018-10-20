[![Project Status: Active The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/SteffenMoritz/imputeR.svg?branch=master)](https://travis-ci.org/SteffenMoritz/imputeR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/steffenmoritz/imputeR?branch=master&svg=true)](https://ci.appveyor.com/project/steffenmoritz/imputeR)
[![CRAN Version](http://www.r-pkg.org/badges/version/imputeR)](https://cran.r-project.org/package=imputeR)
[![License](https://img.shields.io/badge/License-GPL--3-blue.svg
)](https://github.com/SteffenMoritz/imputeR/blob/master/LICENSE.txt)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/imputeR)](https://cran.r-project.org/package=imputeR)
<!-- [![codecov](https://codecov.io/gh/SteffenMoritz/imputeR/branch/master/graph/badge.svg)](https://codecov.io/gh/SteffenMoritz/imputeR) -->

# imputeR: A General Multivariate Imputation Framework <img src="man/figures/imputeR-logo.png" align="right" width="181" height="209" alt="imputeR Logo" />



imputeR is an R package that provides a general framework for missing values imputation based on automated variable selection. 

The main function `impute` inputs a matrix containing missing values and returns a complete data matrix using the variable selection functions provided as part of the package, or written by the user. 

The package also offers many useful tools for imputation research based on `impute`. For example, the `Detect` function can be used to detect the variables' type in a given data matrix. `guess` can be used for naive imputation such as mean imputation, median imputation, majority imputation (for categorical variables only) and random imputation. `SimIm` function stands for "simulation for imputation". It accepts a complete matrix and randomly introduce some percentage of missing values into the matrix so imputation methods can be employed subsequently to impute this artificial missing data matrix. Because the true values are actually know so imputation accuracy can be easily calculated. This calls for the `SimEval` function that extends `SimIm` function, simulates a number of missing data matrices, applies a imputation method to these missing matrices and evaluate its performance. This enables the uncertainty of the imputation method to be obtained. 

### Reference
You can cite imputeR the following: 

  > Feng L, Moritz S, Nowak G, Welsh AH, O'Neill TJ (2018). _imputeR: A
General Multivariate Imputation Framework_. R package version 2.1, <URL:
https://CRAN.R-project.org/package=imputeR>.


### Version
**2.1**

### License
GPL-3
