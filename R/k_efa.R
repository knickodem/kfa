#' k-fold exploratory factor analysis
#'
#' Conducts a sequence of EFAs and converts the resulting factor structure into \code{lavaan} compatible CFA syntax.
#'
#' @param variables a \code{data.frame} of variables (i.e., items) to factor analyze.
#' @param m an integer; maximum number of factors to extract.
#' @param rotation character (case-sensitive); any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package.
#' @param simple logical; Should the simple structure be returned (default)?
#' If \code{FALSE}, items can cross-load on multiple factors.
#' @param threshold numeric between 0 and 1 indicating the minimum (absolute) value
#' of the loading for a variable on a factor. Must be specified when \code{simple = FALSE}
#' @param ordered passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param estimator passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param missing passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param ... other arguments passed to \code{lavaan} functions. See \code{\link[lavaan]{lavOptions}}.
#'
#' @return A list containing \code{lavaan} compatible CFA syntax.
#'
#' @import lavaan
#' @importFrom GPArotation GPFoblq
#' @importFrom GPArotation GPForth
#'
#' @noRd

k_efa <- function(variables, m, rotation,
                  simple, threshold,
                  ordered, estimator, missing, ...){

  ## calculate and extract sample statistics
  sampstats <- sample_stats(variables = variables,
                            ordered = ordered,
                            estimator = estimator,
                            missing = missing,
                            ...)

  ## Running EFAs (no need to run 1-factor b/c we already know the structure)
  efa.loadings <- vector(mode = "list", length = m)

  for(nf in 2:m){

    ## write efa syntax
    efa.mod <- write_efa(nf = nf, vnames = names(variables))

    unrotated <- lavaan::cfa(model = efa.mod,
                             sample.cov = sampstats$cov,
                             sample.nobs = sampstats$nobs,
                             sample.th = sampstats$th,
                             WLS.V = sampstats$wls.v,
                             NACOV = sampstats$nacov,
                             std.lv = TRUE,
                             orthogonal = TRUE,
                             estimator = estimator,
                             missing = missing,
                             parameterization = "delta",
                             se = "none",
                             test = "none")

    # list of unrotated factor loadings
    efa.loadings[[nf]] <- get_std_loadings(unrotated, type = "std.all") # LVs and OVs are standardized
    #lavaan::lavInspect(unrotated, "est")$lambda # LVs are standardized, OVs are not
  }

  # NOTE: Rotation section is different than run_efa rotation section b/c m = 1 model is not run here
  ## if chosen, applying rotation to standardized factor loadings
  # oblique rotations
  if(rotation %in% c("oblimin", "oblimax", "quartimin",
                     "targetQ", "pstQ", "simplimax",
                     "bentlerQ", "geominQ", "cfQ",
                     "infomaxQ", "bifactorQ")){

    f <- function(x){
      try <- tryCatch(expr = GPArotation::GPFoblq(x, method = rotation)$loadings,
                      error = function(e) return(NA))
      out <- if(is.logical(try)) x else try
      return(out)
    }
    loadings <- lapply(efa.loadings[-1], f) # skip the blank element for m = 1


    # orthogonal rotations
  } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
                            "bentlerT", "tandemI", "tandemII",
                            "geominT", "cfT", "infomaxT",
                            "mccammon", "bifactorT")){

    f <- function(x){
      try <- tryCatch(expr = GPArotation::GPForth(x, method = rotation)$loadings,
                      error = function(e) return(NA))
      out <- if(is.logical(try)) x else try
      return(out)
    }
    loadings <- lapply(efa.loadings[-1], f) # skip the blank element for m = 1

  } else {
    loadings <- efa.loadings
    message("Reporting unrotated factor loadings")
  }

  # converting efa results to cfa syntax
  cfa.syntax <- lapply(loadings, function(x){
    efa_cfa_syntax(loadings = x,
                   simple = simple,
                   threshold = threshold,
                   single.item = "none",
                   identified = TRUE,
                   constrain0 = TRUE)
  })

  ## adding the 1-factor model as first element in cfa syntax list
  onefac <- paste0("f1 =~ ", paste(names(variables), collapse = " + "))
  cfa.syntax <- c(list(onefac), cfa.syntax)

  return(cfa.syntax)

}


#' Standardized factor loadings matrix
#'
#' Extract standardized factor loadings from lavaan object
#'
#' @param object a \code{lavaan} object
#' @param type standardize on the latent variables (\code{"std.lv"}),
#' latent and observed variables (\code{"std.all"}, default), or latent and observed variables
#' but not exogenous variables (\code{"std.nox"})? See \code{\link[lavaan]{standardizedSolution}}.
#' @param df should loadings be returned as a \code{matrix} (default) or \code{data.frame}?
#'
#' @return A \code{matrix} or \code{data.frame} of factor loadings
#'
#' @examples
#' data(HolzingerSwineford1939, package = "lavaan")
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#'
#' fit <- lavaan::cfa(HS.model, data = HolzingerSwineford1939)
#' get_std_loadings(fit)
#'
#' @export

get_std_loadings <- function(object, type = "std.all", df = FALSE){

  # extracting unrotated standardized results
  params <- lavaan::standardizedSolution(object, type = type, se = FALSE)
  loaddf <- params[params$op == "=~", c(1, 3, 4)] # drops op column

  # loading matrix dimension names
  inames <- unique(loaddf$rhs) # item names
  fnames <- unique(loaddf$lhs) # factor names

  # wide format
  loads <- stats::reshape(loaddf, direction  = "wide",
                 idvar = "rhs", timevar = "lhs", v.names = "est.std")
  loads[is.na(loads)] <- 0

  if(df == FALSE){
  # matrix of standardized factor loadings
  loads <- as.matrix(loads[-1])
  dimnames(loads) <- list(inames, fnames)
  } else {
    names(loads) <- c("variable", fnames)
  }

  return(loads)
}

#' Gather sample statistics
#'
#' Gather sample statistics for EFA and CFA models using lavCor
#'
#' @param variables a \code{data.frame} of variables to factor analyze.
#' @param ordered passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param estimator passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param missing passed to \code{lavaan} functions. See \code{\link[lavaan]{lavCor}}.
#' @param ... other arguments passed to \code{lavaan} functions. See \code{\link[lavaan]{lavOptions}}.
#'
#' @noRd

sample_stats <- function(variables, ordered, estimator, missing, ...){

  ## calculate and extract sample statistics for test sample
  sampstats <- lavaan::lavCor(object = variables,
                              ordered = ordered,
                              estimator = estimator,
                              missing = missing,
                              output = "fit",
                              cor.smooth = FALSE,
                              ...)

  sample.th <- lavaan::lavInspect(sampstats, "sampstat")$th
  attr(sample.th, "th.idx") <- lavaan::lavInspect(sampstats, "th.idx")

  return(list(fit = sampstats,
              nobs = lavaan::lavInspect(sampstats, "nobs"),
              cov = lavaan::lavInspect(sampstats, "sampstat")$cov,
              # mean = lavaan::lavInspect(sampstats, "sampstat")$mean,
              th = sample.th,
              wls.v = lavaan::lavInspect(sampstats, "wls.v"),
              nacov = lavaan::lavInspect(sampstats, "gamma")))

}
