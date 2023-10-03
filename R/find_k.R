#' Find k for k-fold cross-validation
#'
#' This function is specifically for determining *k* in the context of factor analysis using
#' change in RMSEA as the criterion for identifying the optimal factor model.
#'
#' @param variables a \code{data.frame} (or convertible to a \code{data.frame}) with variables to factor analyze in columns and observations in rows.
#' The power analysis assumes all observations have complete data. Use \code{n} argument or remove rows manually to account for missingness.
#' @param n integer; number of observations. Ignored if \code{variables} is provided.
#' @param p integer; number of variables to factor analyze. Ignored if\code{variables} is provided.
#' @param m integer; maximum number of factors expected to be extracted from \code{variables}. Default is \code{p} / 4 (i.e., 4 variables per factor).
#' @param est.pars integer; number estimated model parameters. Default is 2\code{p}  + \code{m}(\code{m} - 1)/2, which reflects a standardized model with simple structure (e.g., no cross-loadings, higher order factors) or constraints (e.g., tau-equivalence)
#' @param max.k integer; maximum number of folds. Default is 10. \code{NULL} indicates no maximum.
#' @param min.nk integer; minimum sample size per fold. Default is 200 based on simulations from Curran et al. (2003).
#' @param rmsea0 numeric; RMSEA under the null hypothesis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis.
#' @param ... other arguments passed to \code{\link[semTools]{findRMSEAsamplesize}}.
#'
#' @return named vector with the number of folds (k), sample size suggested for each fold by the power analysis (power.nk),
#' the degrees of freedom used for power analysis, and the sample size for each fold used for determining k (nk)--the higher of \code{power.nk} and \code{min.nk}.
#'
#' @references
#' Curran, P. J., Bollen, K. A., Chen, F., Paxton, P., & Kirby, J. B. (2003). Finite sampling properties of the point estimates and confidence intervals of the RMSEA. *Sociological Methods & Research, 32*(2), 208-252. \doi{10.1177/0049124103256130}
#'
#' MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. *Psychological Methods, 1*(2), 130–149. \doi{10.1037/1082-989X.1.2.130}
#'
#' @examples
#' find_k(n = 900, p = 11, m = 3)
#'
#' # adjust precision
#' find_k(n = 900, p = 11, m = 3, rmsea0 = .03, rmseaA = .10)
#'
#' # adjust number of estimated parameters (e.g., constrain all factor loadings to be equal)
#' find_k(n = 900, p = 11, m = 3, est.pars = 15)
#'
#' @importFrom semTools findRMSEAsamplesize
#' @export
#' @md

find_k <- function(variables, n, p, m = NULL, est.pars = NULL, max.k = 10, min.nk = 200,
                  rmsea0 = .05, rmseaA = .08, ...){

  if(!missing(variables)){
    variables <- as.data.frame(variables)
    n <- nrow(variables)
    p <- ncol(variables)
  }

  if(is.null(m)){
    m <- floor(p / 4)
  }

  if(is.null(est.pars)){
    est.pars <- 2*p + m*(m-1)/2  # assumes standardized model identification [factor vars = 1] and no constraints
  }
  # information from cov - estimated parameters
  df <- p*(p+1)/2 - est.pars
  power.nk <- semTools::findRMSEAsamplesize(rmsea0 = rmsea0, rmseaA = rmseaA, df = df, ...)
  # returns sample size
  # defaults are power = .8, alpha = .05, and group = 1 unless changed via ...

  nk <- ifelse(power.nk < min.nk, min.nk, power.nk)

  k <- floor(n / nk)
  if(k < 2){
    message("Power analysis indicates the sample size is too small for k-fold cross validation.
    Adjust assumptions or manually create a single holdout sample.")
  }
  if(!is.null(max.k)){
  k <- ifelse(k > max.k, max.k, k)
  }

  return(c(k = k, power.nk = power.nk, df = df, nk = nk))
}

