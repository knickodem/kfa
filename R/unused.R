#' Aggregate parameter and stability
#'
#' Internal function for aggregating a parameter over k-folds following Rubin's (1987) rules and calculating the stability of the parameter.
#'
#' @param est vector of parameter estimates
#' @param var vector of variances for \code{est}
#' @param n number of observations per fold
#' @param alpha type I error rate for one-sided test
#' @param output.var include all variance components in the output
#'
#' @details
#' The stability index is calculated as 1 - bv / tv where bv is the between fold variance and tv is the total pooled variance.
#'
#' @return a single row \code{data.frame}

agg_param <- function(est, var, n, alpha = .05, output.var = FALSE){

  k <- length(est)
  bar <- mean(est)
  wv <- mean(var)
  bv <- var(est)
  tv <- wv + (1 + k)*bv/k
  stability <- 1 - bv / tv

  # degrees of freedom
  rd <- (1 + 1/k) * bv/tv
  df <- (k - 1) / rd^2
  dfadj <- (n + 1) / (n + 3) * n * (1 - rd)
  df <- df * dfadj / (df + dfadj)

  # confidence interval
  tcrit <- qt(alpha, df)
  ci.lo <- bar - sqrt(tv) * tcrit
  ci.hi <- bar + sqrt(tv) * tcrit

  table <- data.frame(parameter = bar, ci.lo = ci.lo, ci.hi = ci.hi, stability = stability)

  if(output.var == TRUE){
    table <- cbind(table, data.frame(var.within = wv, var.between = bv, var.total = tv))
  }

  return(table)
}

#' Model with best fit
#'
#' Summary table of best fitting model in k-folds
#'
#' @param kfits An object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = TRUE}
#' @param index One or more fit indices to summarize in the report. Default are "chisq", "cfi", and "rmsea".
#'
#' @return \code{data.frame} with fold in rows, index in columns, and factor model with best fit in the cells

best_model <- function(kfits, index = c("chisq", "cfi", "rmsea")){

  k <- length(kfits) # number of folds
  index <- index[index != "df"]

  best.df <- data.frame(fold = 1:k)
  for(i in index){

    if(i %in% c("chisq", "rmsea", "srmr")){

      best <- lapply(kfits, function(x) x[x[[i]] == min(x[[i]]),])

    } else if(i %in% c("cfi", "tli")){

      best <- lapply(kfits, function(x) x[x[[i]] == max(x[[i]]),])

    } else {stop("Not available at the moment")}

    best <- as.data.frame(Reduce(rbind, best))
    best.df <- cbind(best.df, best$factors)
  }

  names(best.df) <- c("fold", index)

  return(best.df)
}
