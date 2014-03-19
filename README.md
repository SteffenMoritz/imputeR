================
Missing values imputation based on automated variable selection

-----
impute is an R package that provides a general framework for missing values imputation based on automated variable selection. It was orignated from part of my PhD thesis and was extended as an R package. 

The main function `impute` inputs a matrix containing missing values and returns a complete data matrix using the variable selection functions provided as part of the package, or written by the user. 

The package also offers many useful tools for imputation research based on `impute`. For example, the `Detect` function can be used to detech the variables' type in a given data matrix. `guess` can be used for naive imputation such as mean imputation, median imputation, majority imputation (for categorical variables only) and random imputation. `SimIm` function stands for "simulation for imputation". It accepts a complete matrix and randomly introduce some percentage of missing values into the matrix so imputation methods can be employed subsequently to impute this artificial missing data matrix. Because the true values are actuallu know so imputation accuracy can be easily calculated. This calls for the `SimEval` function that extends `SimIm` function, simulates a number of missing data matrices, applies a imputation method to these missing matrices and evaluate its performance. This enables the uncertainty of the imputation method to be obtained. 
