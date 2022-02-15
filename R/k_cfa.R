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

  ## calculate and extract sample statistics
  sampstats <- sample_stats(variables = variables,
                            ordered = ordered,
                            estimator = estimator,
                            missing = missing,
                            ...)


  ## run CFAs
  mods <- vector(mode = "list", length = length(syntax))

  for(c in 1:length(syntax)){

    if(nchar(syntax[[c]]) > 0){

      fit <- lavaan::cfa(model = syntax[[c]],
                         sample.cov = sampstats$cov,
                         sample.nobs = sampstats$nobs,
                         sample.th = sampstats$th,
                         WLS.V = sampstats$wls.v,
                         NACOV = sampstats$nacov,
                         estimator = estimator,
                         missing = missing,
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

