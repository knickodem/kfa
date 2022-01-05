#' k-fold confirmatory factor analysis
#'
#' Runs CFA for models identified from a previous EFA.
#'
#' @param syntax A (named) list of \code{lavaan} syntax specifying the CFA models to run.
#' @param variables a \code{data.frame} of variables to factor analyze.
#' @param ordered passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param estimator passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param missing passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param ... other arguments passed to \code{lavaan} functions. See \code{\link[lavaan]{lavOptions}}.
#'
#' @return A list of \code{lavaan} objects
#'
#' @import lavaan
#'
#' @noRd

k_cfa <- function(syntax, variables, ordered, estimator, missing, ...){

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
  sample.th <- lavaan::lavInspect(sampstats, "sampstat")$th
  attr(sample.th, "th.idx") <- lavaan::lavInspect(sampstats, "th.idx")
  WLS.V <- lavaan::lavInspect(sampstats, "wls.v")
  NACOV <- lavaan::lavInspect(sampstats, "gamma")


  ## run CFAs
  mods <- vector(mode = "list", length = length(syntax))

  for(c in 1:length(syntax)){

    if(nchar(syntax[[c]]) > 0){

      fit <- lavaan::cfa(model = syntax[[c]],
                         sample.cov = sample.cov,
                         sample.nobs = sample.nobs,
                         sample.th = sample.th,
                         WLS.V = WLS.V,
                         NACOV = NACOV,
                         estimator = estimator,
                         parameterization = "delta")

      mods[[c]] <- fit

    } else {

      mods[[c]] <- NULL
    }

  }

  mods <- mods[lengths(mods) != 0] # dropping NULL elements
  names(mods) <- names(syntax)[which(nchar(syntax) > 0)] # adding names

  return(mods)
}

