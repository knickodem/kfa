#' k-fold exploratory factor analysis
#'
#' For each k fold, the function conducts a sequence of EFAs and
#' converts the resulting factor structure into \code{lavaan}
#' compatible CFA syntax.
#'
#' @param variables dataframe of variables to factor analyze
#' @param m integer; maximum number of factors to extract.
#' @param rotation character (case-sensitive); any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package.
#' @param ordered passed to lavaan functions
#' @param estimator passed to lavaan functions
#' @param missing passed to lavaan functions
#'
#' @return A list containing \code{lavaan} compatible CFA syntax.

k_efa <- function(variables, m, rotation,
                  ordered, estimator, missing, ...){

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

  ## Running EFAs (no need to run 1-factor b/c we already know the structure)
  efa.loadings <- vector(mode = "list", length = m)

  for(nf in 2:m){

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
  }

  ## if chosen, applying rotation to standardized factor loadings for models where m > 1
  # oblique rotations
  if(rotation %in% c("oblimin", "oblimax", "quartimin",
                     "targetQ", "pstQ", "simplimax",
                     "bentlerQ", "geominQ", "cfQ",
                     "infomaxQ", "bifactorQ")){

    loadings <- lapply(efa.loadings[-1], function(x){
      GPArotation::GPFoblq(x, method = rotation)$loadings
    })

    # orthogonal rotations
  } else if(rotation %in% c("targetT", "pstT", "entropy","quartimax", "varimax",
                            "bentlerT", "tandemI", "tandemII",
                            "geominT", "cfT", "infomaxT",
                            "mccammon", "bifactorT")){

    loadings <- lapply(efa.loadings[-1], function(x){
      GPArotation::GPForth(x, method = rotation)$loadings
    })

  } else {
    loadings <- efa.loadings[-1]
    message("Reporting unrotated factor loadings")
  }

  # converting efa results to cfa syntax
  cfa.syntax <- lapply(loadings, function(x){
    efa_cfa_syntax(loadings = x,
                   simple = TRUE,
                   threshold = NA,
                   single.item = "")
  })

  ## adding the 1-factor model as first element in cfa syntax list
  onefac <- paste0("f1 =~ ", paste(names(variables), collapse = " + "))
  cfa.syntax <- c(list(onefac), cfa.syntax)

  return(cfa.syntax)

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
