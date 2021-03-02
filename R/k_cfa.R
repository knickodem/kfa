#' k-fold confirmatory factor analysis
#'
#' Runs CFA for models identified from a previous EFA.
#'
#' @param efa An object returned from \code{\link[kfa]{k_efa}}
#' @param variables a \code{data.frame} of variables to factor analyze.
#' @param ordered passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param estimator passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param missing passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param ... other arguments passed to \code{lavaan} functions. See \code{\link[lavaan]{lavOptions}}.
#'
#' @return A list of \code{lavaan} objects

k_cfa <- function(efa, variables, ordered, estimator, missing, ...){

  ## calculate and extract sample statistics for test sample
  sampstats <- lavaan::lavCor(object = variables,
                              ordered = ordered,
                              estimator = estimator,
                              missing = missing,
                              output = "fit",
                              cor.smooth = FALSE,
                              ...)

  sample.nobs <- lavaan::lavInspect(sampstats, "nobs")
  sample.cov <- lavaan::lavInspect(sampstats, "sampstat")$cov
  sample.mean <- lavaan::lavInspect(sampstats, "sampstat")$mean
  sample.th <- lavaan::lavInspect(sampstats, "sampstat")$th
  attr(sample.th, "th.idx") <- lavaan::lavInspect(sampstats, "th.idx")
  WLS.V <- lavaan::lavInspect(sampstats, "wls.v")
  NACOV <- lavaan::lavInspect(sampstats, "gamma")


  ## run CFAs
  cfa <- vector(mode = "list", length = length(efa))

  for(c in 1:length(efa)){

    if(nchar(efa[[c]]) > 0){

      fit <- lavaan::cfa(model = efa[[c]],
                         sample.cov = sample.cov,
                         sample.nobs = sample.nobs,
                         sample.mean = sample.mean,
                         sample.th = sample.th,
                         WLS.V = WLS.V,
                         NACOV = NACOV,
                         estimator = estimator,
                         parameterization = "delta")

      cfa[[c]] <- fit

    } else {

      cfa[[c]] <- NULL
    }

  }

  return(cfa)
}

