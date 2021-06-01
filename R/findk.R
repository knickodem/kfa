#' Find k for k-fold cross-validation
#'
#' This function is specifically for determining *k* in the context of factor analysis using
#' change in RMSEA as the criterion for identifying the optimal factor model.
#'
#' @param variables a \code{data.frame} (or convertible to a \code{data.frame}) of variables to factor analyze
#' @param m integer; number of factors. Set to the maximum number of factors expected to be extracted from \code{variables}. Default is 4 items per factor.
#' @param maxk integer; maximum number of folds. \code{NULL} indicates no maximum.
#' @param rmsea0 numeric; RMSEA under the null hypothesis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis.
#' @param ... other arguments passed to \code{\link[semTools]{findRMSEAsamplesize}}.
#'
#' @return integer indicating the number of folds
#'
#' @references
#' Curran, P. J., Bollen, K. A., Chen, F., Paxton, P., & Kirby, J. B. (2003). Finite sampling properties of the point estimates and confidence intervals of the RMSEA. *Sociological Methods & Research, 32*(2), 208-252.
#'
#' @export

findk <- function(variables, m = floor(ncol(variables) / 4), maxk = 10,
                  rmsea0 = .05, rmseaA = .08, ...){
  # Ideally, need a better rationale for default rmsea0 and rmseaA.

  variables <- as.data.frame(variables)
  p <- ncol(variables)
  df <- ((p - m)^2 - (p + m)) / 2

  samp <- semTools::findRMSEAsamplesize(rmsea0 = rmsea0, rmseaA = rmseaA, df = df, ...)
  # unless changed via ...defaults are power = .8, alpha = .05, and group = 1

  k <- floor(nrow(variables) / samp)
  if(k < 2){
    message("Power analysis indicates the sample size is too small for
         k-fold cross validation. Adjust assumptions or manually create a single
         holdout sample.")
  }
  if(!is.null(maxk)){
  k <- ifelse(k > maxk, maxk, k)
  }

  return(k)
}
