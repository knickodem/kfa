# kfa
k-fold cross validation for factor analysis

For a set of variables, this package includes utilities to evaluate and compare models 
across k folds to identify an optimal factor structure.

## Process

1. If k is not specified, calculate sample size needed for sufficient power.
2. Create k folds (i.e., training and testing samples) based on user specification 
   or power analysis.
3. For each fold, calculate the sample statistics (e.g., correlation matrix, 
   thresholds \[if necessary]) from training sample.
4. Run 2:m factor exploratory factor analysis (EFA) models using the sample statistics, 
   apply rotation (if specified), and define structure for a confirmatory factor 
   analysis (CFA). The structure for a 1-factor CFA is also defined.
5. Run the 1:m factor CFA models on the testing sample and compare model fit 
   primarily using RMSEA.
6. Summarize results for each 1:m factor model by averaging the parameter
   estimates and fit indices across folds.