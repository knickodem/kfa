#' Conducts exploratory factor analysis
#'
#' This function is intended for use on independent samples rather than integrated
#' with k-fold cross-validation.
#'
#' @inheritParams efa_cfa_syntax
#' @inheritParams kfa
#'
#' @details
#' When converting EFA results to CFA syntax (via \code{\link[kfa]{efa_cfa_syntax}}), the simple structure is
#' defined as each variable loading onto a single factor. This is determined using the largest factor loading for each variable.
#' When \code{simple = FALSE}, variables are allowed to cross-load on multiple factors. In this case, all pathways with loadings
#' above the \code{min.loading} are retained. However, allowing cross-loading variables can result in model under-identification.
#' An identification check is run by default, but can be turned off by setting \code{identified = FALSE}.
#'
#' @return A three-element \code{list}:
#' \itemize{
#' \item **efas** \code{lavaan} object for each *m* model
#' \item **loadings** (rotated) factor loading matrix for each *m* model
#' \item **cfa.syntax** CFA syntax generated from loadings
#' }
#'
#' @references
#' Millsap, R. E. (2001). When trivial constraints are not trivial: The choice of uniqueness constraints in confirmatory factor analysis. *Structural Equation Modeling, 8*(1), 1-17. \doi{10.1207/S15328007SEM0801_1}
#'
#' @examples
#'
#' # simulate data based on a 3-factor model with standardized loadings
#' sim.mod <- "f1 =~ .7*x1 + .8*x2 + .3*x3 + .7*x4 + .6*x5 + .8*x6 + .4*x7
#'                 f2 =~ .8*x8 + .7*x9 + .6*x10 + .5*x11 + .5*x12 + .7*x13 + .6*x14
#'                 f3 =~ .6*x15 + .5*x16 + .9*x17 + .4*x18 + .7*x19 + .5*x20
#'                 f1 ~~ .2*f2
#'                 f2 ~~ .2*f3
#'                 f1 ~~ .2*f3
#'                 x9 ~~ .2*x10"
#' set.seed(1161)
#' sim.data <- simstandard::sim_standardized(sim.mod, n = 900,
#'                                           latent = FALSE,
#'                                           errors = FALSE)[c(2:9,1,10:20)]
#' # Run 1-, 2-, and 3-factor models
#' efas <- run_efa(sim.data, m = 3)
#'
#' @export
#' @md

run_efa <- function(data, variables = names(data), m = floor(ncol(data) / 4), rotation = "oblimin",
                    simple = TRUE, min.loading = NA, single.item = c("keep","drop", "none"),
                    identified = TRUE, constrain0 = FALSE,
                    ordered = FALSE, estimator = NULL, missing = "listwise", ...){

  data <- as.data.frame(data)

  # The ordered = TRUE functionality not available in lavCor (i.e., not currently equivalent to listing
  # all items), so need to do it manually since I want this functionality for our users
  if(is.logical(ordered)){
    if(ordered == FALSE){
      ordered <- NULL
      if(is.null(estimator)){
        estimator <- "MLMVS"
      }
    } else if(ordered == TRUE){
      ordered <- variables
      if(is.null(estimator)){
        estimator <- "WLSMV"
      }
    }
  } else if(is.character(ordered)){
    if(is.null(estimator)){
      estimator <- "WLSMV"
    }
  }

  ## calculate and extract sample statistics
  sampstats <- sample_stats(data = data,
                            variables = variables,
                            ordered = ordered,
                            estimator = estimator,
                            missing = missing,
                            ...)


  ## Running EFAs, comparing models, converting structure to CFA syntax
  # results objects
  lav.objects <- vector(mode = "list", length = m)
  efa.loadings <- vector(mode = "list", length = m)
  # mod.compare <- data.frame()

  for(nf in 1:m){

    ## write efa syntax
    efa.mod <- write_efa(nf = nf, vnames = variables)

    lav.objects[[nf]] <- lavaan::cfa(model = efa.mod,
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
    efa.loadings[[nf]] <- get_std_loadings(lav.objects[[nf]], type = "std.all")
      #lavaan::lavInspect(lav.objects[[nf]], "est")$lambda
  }

  # NOTE: Rotation section is different than k_efa rotation section b/c m = 1 model is run here
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
    loadings <- c(list(efa.loadings[[1]]), lapply(efa.loadings[-1], f))

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
    loadings <- c(list(efa.loadings[[1]]), lapply(efa.loadings[-1], f))

  } else {
    loadings <- efa.loadings
    message("Reporting unrotated factor loadings")
  }

  # converting efa results to cfa syntax
  cfa.syntax <- lapply(loadings, function(x){
    efa_cfa_syntax(loadings = x,
                   simple = simple,
                   min.loading = min.loading,
                   single.item = single.item,
                   identified = identified,
                   constrain0 = constrain0)
  })

  efaout <- list(efas = lav.objects,
                 loadings = loadings,
                 cfa.syntax = cfa.syntax)
                 # mod.compare = mod.compare, # data.frame of model comparisons results
  return(efaout)
}



# ## model comparison code
# if(nf > 1){
#   ## compare models
#   comp <- lavaan::lavTestLRT(lav.objects[[nf-1]], lav.objects[[nf]])
#
#   ## save results
#   tempdf <- data.frame(model = paste0(nf - 1, " v. ", nf),
#                        chisq.diff = comp[2, 5],
#                        df.diff = comp[2, 6],
#                        p.value = comp[2, 7])
#
#   mod.compare <- rbind(mod.compare, tempdf)
# }
