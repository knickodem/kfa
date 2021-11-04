#' Conducts exploratory factor analysis
#'
#' This function is intended for use on independent samples rather than integrated
#' with k-fold cross-validation.
#'
#' @param variables a \code{data.frame} (or convertible to a \code{data.frame}) of variables to factor analyze
#' @param rotation character (case-sensitive); any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package.
#' Default is "oblimin".
#' @param m integer; maximum number of factors to extract. Default is 4 items per factor.
#' @param simple logical; Should the most simple structure be returned (default)?
#' If \code{FALSE}, items can cross load on multiple factors.
#' @param threshold numeric between 0 and 1 indicating the minimum (absolute) value
#' of the loading for an item on a factor.
#' @param single.item character indicating how single-item factors should be treated.
#' Use \code{"keep"} (default) to keep them in the model when generating the CFA syntax, \code{"drop"}
#' to remove them, or \code{"none"} indicating the CFA syntax should not be generated for
#' this model and \code{""} will be returned.
#' @param ordered logical; Should items be treated as ordinal and the
#' polychoric correlations used in the factor analysis? When \code{FALSE} (default)
#' the Pearson correlation matrix is used. A character vector of item names is
#' also accepted to prompt estimation of the polychoric correlation matrix.
#' @param estimator if \code{ordered = FALSE}, the default is "ML". If
#' \code{ordered = TRUE}, the default is "DWLS". See \code{\link[lavaan]{lavaan}} for other options.
#' @param missing default is "listwise". See \code{\link[lavaan]{lavaan}} for other options.
#' @param ... other arguments passed to \code{lavaan} functions. See \code{\link[lavaan]{lavOptions}}.
#'
#' @return A three-element \code{list}:
#' \itemize{
#' \item **efas** \code{lavaan} object for each *m* model
#' \item **loadings** (rotated) factor loading matrix for each *m* model
#' \item **cfa.syntax** CFA syntax generated from loadings
#' }
#'
#' @export

run_efa <- function(variables, m = floor(ncol(variables) / 4), rotation = "oblimin",
                    simple = TRUE, threshold = NA, single.item = c("keep","drop", ""),
                    ordered = FALSE, estimator = NULL, missing = "listwise", ...){

  variables <- as.data.frame(variables)

  # The ordered = TRUE functionality in lavaan is not currently equivalent to listing
  # all items, so need to do it manually since I want this functionality for our users
  if(is.character(ordered)){
    if(is.null(estimator)){
      estimator <- "DWLS"
    }
  }
  if(ordered == TRUE){
    ordered <- names(variables)
    if(is.null(estimator)){
      estimator <- "DWLS"
    }
  } else if(ordered == FALSE){
    ordered <- NULL
    if(is.null(estimator)){
      estimator <- "ML"
    }
  }

  ## calculate and extract sample statistics
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


  ## Running EFAs, comparing models, converting structure to CFA syntax
  # results objects
  efa.loadings <- vector(mode = "list", length = m)
  # mod.compare <- data.frame()

  for(nf in 1:m){

    ## write efa syntax
    efa.mod <- write_efa(nf = nf, vnames = names(variables))

    unrotated <- lavaan::cfa(model = efa.mod,
                             sample.cov = sample.cov,
                             sample.nobs = sample.nobs,
                             sample.mean = sample.mean,
                             sample.th = sample.th,
                             WLS.V = WLS.V,
                             NACOV = NACOV,
                             std.lv = TRUE,
                             orthogonal = TRUE,
                             estimator = estimator,
                             missing = missing,
                             parameterization = "delta",
                             se = "none",
                             test = "none")

    # list of unrotated factor loadings
    efa.loadings[[nf]] <- lavaan::lavInspect(unrotated, "est")$lambda
  }

  ## if chosen, applying rotation to standardized factor loadings for models where m > 1
  # oblique rotations
  if(rotation %in% c("oblimin", "oblimax", "quartimin",
                     "targetQ", "pstQ", "simplimax",
                     "bentlerQ", "geominQ", "cfQ",
                     "infomaxQ", "bifactorQ")){

    f <- function(x){
      try <- tryCatch(expr = GPArotation::GPFoblq(x, method = rotation)$loadings,
                      error = function(e) return(NA))
      out <- if(is.na(try)){x} else {try}
      return(out)
    }
    loadings <- lapply(efa.loadings[-1], f)

    # orthogonal rotations
  } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
                            "bentlerT", "tandemI", "tandemII",
                            "geominT", "cfT", "infomaxT",
                            "mccammon", "bifactorT")){

    f <- function(x){
      try <- tryCatch(expr = GPArotation::GPForth(x, method = rotation)$loadings,
                      error = function(e) return(NA))
      out <- if(is.na(try)){x} else {try}
      return(out)
    }
    loadings <- lapply(efa.loadings[-1], f)

  } else {
    loadings <- efa.loadings
    message("Reporting unrotated factor loadings")
  }

  # converting efa results to cfa syntax
  cfa.syntax <- lapply(loadings, function(x){
    efa_cfa_syntax(loadings = x,
                   simple = simple,
                   threshold = threshold,
                   single.item = single.item)
  })

  efaout <- list(efas = unrotated,
                 loadings = loadings,       # list of rotated loadings as GPArotation object
                 cfa.syntax = cfa.syntax)
                 # mod.compare = mod.compare, # data.frame of model comparisons results


  return(efaout)

}



# ## model comparison code
# if(nf > 1){
#   unrotated.a <- unrotated
# }
# if(nf > 1){
#   ## compare models
#   comp <- lavaan::lavTestLRT(unrotated.a, unrotated)
#
#   ## save results
#   tempdf <- data.frame(model = paste0(nf - 1, " v. ", nf),
#                        chisq.diff = comp[2, 5],
#                        df.diff = comp[2, 6],
#                        p.value = comp[2, 7])
#
#   mod.compare <- rbind(mod.compare, tempdf)
# }
