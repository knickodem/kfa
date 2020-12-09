#' Conducts exploratory factor analysis
#'
#' This function is intended for use on independent samples rather than integrated
#' with k-fold cross-validation.
#'
#' @param variables dataframe of variables to factor analyze
#' @param rotation character (case-sensitive); any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package.
#' Default is "oblimin".
#' @param m integer; maximum number of factors to extract. Default is 4 items per factor.
#' @param simple logical; Should the most simple structure be returned (default)?
#' If \code{FALSE}, items can cross load on multiple factors.
#' @param threshold numeric between 0 and 1 indicating the minimum (absolute) value
#' of the loading for an item on a factor.
#' @param single.item character indicating how single-item factors should be treated.
#' Use "keep" to keep them in the model when generating the CFA syntax, "drop"
#' to remove them, or "" indicating the CFA syntax should not be generated for this model.
#' @param ordered logical; Should items be treated as ordinal and the
#' polychoric correlations used in the factor analysis? When \code{FALSE} (default)
#' the Pearson correlation matrix is used. A character vector of item names is
#' also accepted to prompt estimation of the polychoric correlation matrix.
#' @param missing passed to \code{lavaan} functions
#' @param ... other arguments to pass on to \code{lavaan}

run_efa <- function(variables, m = floor(ncol(items) / 4), rotation = "oblimin",
                    simple = TRUE, threshold = NA, single.item = c("keep","drop", ""),
                    ordered = FALSE, estimator = NULL, missing = "listwise", ...){

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
  sampstats <- lavaan::lavCor(variables,
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

  # returns a data frame and summary information
  nfactors <- parameters::n_factors(x = variables,
                                    type = "FA",
                                    rotation = rotation,
                                    package = c("psych", "nFactors"),
                                    cor = sample.cov,
                                    safe = TRUE)


  ## Running EFAs, comparing models, converting structure to CFA syntax
  # results objects
  efa.loadings <- vector(mode = "list", length = m)
  mod.compare <- data.frame()

  for(nf in 1:m){
    if(nf > 1){
      unrotated.a <- unrotated
    }

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
                             parameterization = "delta",
                             se = "none",
                             test = "none")

    # list of unrotated factor loadings
    efa.loadings[[nf]] <- lavaan::lavInspect(unrotated, "est")$lambda

    if(nf > 1){
      ## compare models
    comp <- lavaan::lavTestLRT(unrotated.a, unrotated)

    ## save results
    tempdf <- data.frame(model = paste0(nf - 1, " v. ", nf),
                         chisq.diff = comp[2, 5],
                         df.diff = comp[2, 6],
                         p.value = comp[2, 7])

    mod.compare <- rbind(mod.compare, tempdf)
    }
  }

  ## if chosen, applying rotation to standardized factor loadings for models where m > 1
  # oblique rotations
  if(rotation %in% c("oblimin", "oblimax", "quartimin",
                     "targetQ", "pstQ", "simplimax",
                     "bentlerQ", "geominQ", "cfQ",
                     "infomaxQ", "bifactorQ")){

    loadings <- lapply(efa.loadings, function(x){
      if(ncol(x) > 1){
        GPArotation::GPFoblq(x, method = rotation)$loadings
      } else {x}
    })

    # orthogonal rotations
  } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
                            "bentlerT", "tandemI", "tandemII",
                            "geominT", "cfT", "infomaxT",
                            "mccammon", "bifactorT")){

    loadings <- lapply(efa.loadings, function(x){
      if(ncol(x) > 1){
        GPArotation::GPForth(x, method = rotation)$loadings
      } else {x}
    })

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

  efaout <- list(syntax = cfa.syntax,
                 mod.compare = mod.compare, # data.frame of model comparisons results
                 loadings = loadings,       # list of rotated loadings as GPArotation object
                 nfactors = nfactors)       # data.frame of consensus factor extraction

  return(efaout)

}



## internal function for extracting standardized loadings
get_std_loadings <- function(object, type = "std.all"){

  # extracting unrotated standardized results
  params <- lavaan::standardizedsolution(object, type = type,
                                         se = FALSE, zstat = FALSE, # Not needed so saves
                                         pvalue = FALSE, ci = FALSE)# computation time
  loaddf <- params[params$op == "=~",]

  # loading matrix dimension names
  inames <- unique(loaddf$rhs) # item names
  fnames <- unique(loaddf$lhs) # factor names

  # matrix of standardized factor loadings
  loadmat <- matrix(loaddf$est.std,
                    ncol = length(fnames), nrow = length(inames),
                    byrow = FALSE, dimnames = list(inames, fnames))

  return(loadmat)
}
