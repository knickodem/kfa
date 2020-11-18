#' Determine k for k-fold cross-validation
#' 
#' This function is specifically for determining k in the context of factor analysis. 
#' Using change in RMSEA as the criterion for obtaining the best fitting factor
#' analytic model, the function determines the number of folds based on the sample
#' size, maximum number of possible factors, and RMSEA values.
#' 

# Ideally, need a better rationale for default rmsea0 and rmseaA.
findk <- function(items, m, rmsea0 = .05, rmseaA = .08, ...){
  
  
  p <- ncol(items)
  df <- ((p - m)^2 - (p + m)) / 2
  
  # # equivalently:
  # info <- p * (p + 1) / 2
  # # factor loadings + factor vars/covs + residual vars
  # params <- (p * m) + ((m * (m + 1)) / 2) +(p - m^2)
  # df <- info - params
  
  samp <- semTools::findRMSEAsamplesize(rmsea0 = rmsea0, rmseaA = rmseaA, df = df, ...)
  # defaults are power = .8, alpha = .05, and group = 1 unless changed via ...
  
  k <- floor(nrow(items)/ samp)
  k <- ifelse(k < 1, 1, ifelse(k > 10, 10, k)) # probably don't need more than 10 folds, plus it would take forever
  
  return(k)
  
}

