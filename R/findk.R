#' Determine k for k-fold cross-validation
#'
#' This function is specifically for determining k in the context of factor analysis.
#' Using change in RMSEA as the criterion for obtaining the best fitting factor
#' analytic model, the function determines the number of folds based on the sample
#' size, maximum number of possible factors, and RMSEA values.
#'
#' @param p integer specifying the number of items or \code{data.frame}
#' where the number of items is determined via {\link[base]{ncol}}
#' @param m integer; maximum number of factors to extract. Default is 4 items per factor.
#' @param rmsea0 numeric; RMSEA under the null hypothesis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis.
#'
#' @return numeric indicating the number of folds
#'

# Ideally, need a better rationale for default rmsea0 and rmseaA.
findk <- function(variables, m, rmsea0 = .05, rmseaA = .08, ...){

  if(is.data.frame(variables)){
    p <- ncol(variables)
  } else{
    stop("variables must be a dataframe (for now).")
  }

  df <- ((p - m)^2 - (p + m)) / 2

  samp <- semTools::findRMSEAsamplesize(rmsea0 = rmsea0, rmseaA = rmseaA, df = df, ...)
  # defaults are power = .8, alpha = .05, and group = 1 unless changed via ...

  k <- floor(nrow(variables) / samp)
  if(k < 2){
    stop("Power analysis indicates the sample size is too small for
         k-fold cross validation. Adjust assumptions or manually create a single
         holdout sample.")
  }
  k <- ifelse(k > 10, 10, k) # probably don't need > 10 folds, plus it would take forever

  return(k)

}


# # equivalently:
# info <- p * (p + 1) / 2
# # factor loadings + factor vars/covs + residual vars
# params <- (p * m) + ((m * (m + 1)) / 2) +(p - m^2)
# df <- info - params
