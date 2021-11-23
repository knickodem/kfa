#' Find k for k-fold cross-validation
#'
#' This function is specifically for determining *k* in the context of factor analysis using
#' change in RMSEA as the criterion for identifying the optimal factor model.
#'
#' @param variables a \code{data.frame} (or convertible to a \code{data.frame}) with variables to factor analyze in columns and observations in rows.
#' @param n integer; number of observations. Ignored if \code{variables} is provided.
#' @param p integer; number of variables to factor analyze. Ignored if\code{variables} is provided.
#' @param m integer; maximum number of factors expected to be extracted from \code{variables}. Default is \code{p} / 4 (i.e., 4 variables per factor).
#' @param max.k integer; maximum number of folds. Default is 10. \code{NULL} indicates no maximum.
#' @param min.n integer; minimum sample size per fold. Default is 200 based on simulations from Curran et al. (2003).
#' @param rmsea0 numeric; RMSEA under the null hypothesis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis.
#' @param ... other arguments passed to \code{\link[semTools]{findRMSEAsamplesize}}.
#'
#' @return named vector with the number of folds, sample size suggested by the power analysis, and the actual sample size used for determining k.
#'
#' @references
#' Curran, P. J., Bollen, K. A., Chen, F., Paxton, P., & Kirby, J. B. (2003). Finite sampling properties of the point estimates and confidence intervals of the RMSEA. *Sociological Methods & Research, 32*(2), 208-252.
#'
#' MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. *Psychological Methods, 1*(2), 130–149. doi: 10.1037/1082989X.1.2.130
#'
#' @examples
#' find_k(n = 900, p = 20, m = 3)
#'
#' @importFrom semTools findRMSEAsamplesize
#' @export

find_k <- function(variables, n, p, m = NULL, max.k = 10, min.n = 200,
                  rmsea0 = .05, rmseaA = .08, ...){

  if(!missing(variables)){
    variables <- as.data.frame(variables)
    n <- nrow(variables)
    p <- ncol(variables)
  }

  if(is.null(m)){
    m <- floor(p / 4)
  }

  df <- ((p - m)^2 - (p + m)) / 2
  power.n <- semTools::findRMSEAsamplesize(rmsea0 = rmsea0, rmseaA = rmseaA, df = df, ...)
  # returns sample size
  # defaults are power = .8, alpha = .05, and group = 1 unless changed via ...

  actual.n <- ifelse(power.n < min.n, min.n, power.n)

  k <- floor(n / actual.n)
  if(k < 2){
    message("Power analysis indicates the sample size is too small for k-fold cross validation.
    Adjust assumptions or manually create a single holdout sample.")
  }
  if(!is.null(max.k)){
  k <- ifelse(k > max.k, max.k, k)
  }

  return(c(k = k, power.n = power.n, actual.n = actual.n))
}

