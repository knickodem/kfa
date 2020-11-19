#' Conducts exploratory factor analysis
#'
#' This function is intended for use on independent samples rather than integrated
#' with k-fold cross-validation.
#'
#' @param items dataframe of item responses
#' @param rotation character; any rotation method listed in \code{\link[GPArotation]{rotations}}
#' in the \code{GPArotation} package. Default is "oblimin".
#' @param ordered logical; Should variables be treated as ordinal and the polychoric correlations used in the factor analysis?
#'   Default is FALSE where all variables are considered numeric and the Pearson correlation matrix is calculated.
#' A character vector of variable names is also accepted to prompt estimation of the polychoric correlation matrix.
#' @param missing passed to lavaan functions
#' @param ... other arguments to pass on to lavaan

run_efa <- function(items, rotation, ordered, missing, ...){

  ## identify optimal number of factors using up to 20 methods
    # calculate correlation matrix to pass to n_factors()
    cor.lv <- lavaan::lavCor(items,
                             ordered = ordered,
                             missing = missing,
                             output = "cor",
                             cor.smooth = FALSE,
                             ...)


    # in package argument,
    # "psych" runs the fit (4 methods) family: RMSEA, TLI, BIC, CRMS
    # "nFactors" runs Bartlett (3), Bentler (1), CNG (1), Multiple Regression (3) and
    # Scree (6) families of methods
    # "EGAnet" runs the EGA (2) family: glasso and TMFG
    # Need to decide which, if not all, methods we want to consider using for our criteria

    ## How many factors should be extracted?
    # returns a data frame and summary information
    extractm <- parameters::n_factors(x = items,
                                      type = "FA",
                                      rotation = rotation,
                                      package = c("psych", "nFactors"),
                                      cor = cor.lv,
                                      safe = TRUE)

    # the consensus for number of factors (ties go to smallest n)
    nf <- attr(extractm, "n")

    ## efa for 1 factor
    unrotated.a <- semTools::efaUnrotate(data = items,
                                         nf = 1,
                                         start = FALSE,
                                         ordered = ordered,
                                         missing = missing,
                                         parameterization = "theta",
                                         ...)

    # insert tryCatch for problems with initial run

    ## iteratively compare 2 factor through m factor models
    extractm <- data.frame()
    nf <- 2
    while(nf <= m){

      if(nf > 2){
        unrotated.a <- unrotated.b
      }

      unrotated.b <- semTools::efaUnrotate(data = items,
                                           nf = nf,
                                           start = FALSE,
                                           ordered = ordered,
                                           missing = missing,
                                           parameterization = "theta",
                                           ...)

      ## compare models
      comp <- lavaan::lavTestLRT(unrotated.a, unrotated.b)

      ## save results
      tempdf <- data.frame(model = paste0(nf - 1, " v. ", nf),
                           chisq.diff = comp[2, 5],
                           df.diff = comp[2, 6],
                           p.value = comp[2, 7])
      extractm <- rbind(extractm, tempdf)

      ## keep smaller model if non-significant
      if(comp[2,7] > .05){
        unrotated <- unrotated.a
        break

      } else {

        nf <- nf + 1
        unrotated <- unrotated.b

      }
    }
    # number of factors in best model
    nf <- nf - 1


efa.loadings <- vector(mode = "list", length = m)

for(nf in 1:m){

  unrotated <- semTools::efaUnrotate(data = items,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = ordered,
                                     missing = missing,
                                     parameterization = "theta")
  # list of unrotated factor loadings
  efa.loadings[[nf]] <- get_std_loadings(unrotated, type = "std.all")
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
                 simple = TRUE,
                 threshold = threshold,
                 single.item = "")
})

  efaout <- list(unrotated = unrotated, # best model as lavaan object
              rotated = rotated,        # rotated parameters as GPArotation object
              nf = nf,                  # number of factors in best model
              extractm = extractm)      # summary of extraction results

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
