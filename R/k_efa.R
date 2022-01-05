#' k-fold exploratory factor analysis
#'
#' Conducts a sequence of EFAs and converts the resulting factor structure into \code{lavaan} compatible CFA syntax.
#'
#' @param variables a \code{data.frame} of variables to factor analyze.
#' @param m an integer; maximum number of factors to extract.
#' @param rotation character (case-sensitive); any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package.
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
                  ordered, estimator, missing, ...){

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
  # below not really necessary with ML estimator
  sample.th <- lavaan::lavInspect(sampstats, "sampstat")$th
  attr(sample.th, "th.idx") <- lavaan::lavInspect(sampstats, "th.idx")
  WLS.V <- lavaan::lavInspect(sampstats, "wls.v")
  NACOV <- lavaan::lavInspect(sampstats, "gamma")

  ## Running EFAs (no need to run 1-factor b/c we already know the structure)
  efa.loadings <- vector(mode = "list", length = m)

  for(nf in 2:m){

    ## write efa syntax
    efa.mod <- write_efa(nf = nf, vnames = names(variables))

    unrotated <- lavaan::cfa(model = efa.mod,
                             sample.cov = sample.cov,
                             sample.nobs = sample.nobs,
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
    efa.loadings[[nf]] <- get_std_loadings(unrotated, type = "std.all") # LVs and OVs are standardized
    #lavaan::lavInspect(unrotated, "est")$lambda # LVs are standardized, OVs are not

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
      out <- if(is.logical(try)) x else try
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
      out <- if(is.logical(try)) x else try
      return(out)
    }
    loadings <- lapply(efa.loadings[-1], f)

  } else {
    loadings <- efa.loadings[-1]
    message("Reporting unrotated factor loadings")
  }

  # converting efa results to cfa syntax
  cfa.syntax <- lapply(loadings, function(x){
    efa_cfa_syntax(loadings = x,
                   simple = TRUE,
                   threshold = NA,
                   single.item = "none")
  })

  ## adding the 1-factor model as first element in cfa syntax list
  onefac <- paste0("f1 =~ ", paste(names(variables), collapse = " + "))
  cfa.syntax <- c(list(onefac), cfa.syntax)

  return(cfa.syntax)

}


#' standardized factor loadings
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
#' @export

get_std_loadings <- function(object, type = "std.all", df = FALSE){

  # extracting unrotated standardized results
  params <- lavaan::standardizedSolution(object, type = type, se = FALSE)
  loaddf <- params[params$op == "=~", c(1, 3, 4)] # drops op column

  # loading matrix dimension names
  inames <- unique(loaddf$rhs) # item names
  fnames <- unique(loaddf$lhs) # factor names

  # wide format
  loads <- reshape(loaddf, direction  = "wide",
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
