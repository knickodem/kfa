#' k-fold exploratory factor analysis
#'
#' For each k fold, the function conducts a sequence of EFAs and
#' converts the resulting factor structure into \code{lavaan}
#' compatible CFA syntax.
#'
#' @param items dataframe of item responses
#' @param efa.method character; How should the EFA models be determined?
#' "sequence" sequentially runs 1 to *m* factor models.
#' "consensus" calls \code{\link[parameters]{n_factors}} which identifies the
#' optimal number of factors based the consensus of a variety of methods.
#' @param rotation character; any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package.
#' @param m integer; maximum number of factors to extract.
#' @param ordered passed to lavaan functions
#' @param missing passed to lavaan functions
#'
#' @return A list containing \code{lavaan} compatible CFA syntax.

k_efa <- function(items, efa.method, rotation, m, threshold, ordered, missing, ...){

  ## Determine the optimal number of factors
  if(efa.method == "consensus"){

    ## calculate correlation matrix - needed for n_factors()
    cor.lv <- lavaan::lavCor(items,
                             ordered = ordered,
                             missing = missing,
                             output = "cor",
                             cor.smooth = FALSE,
                             ...)

    # returns a data frame and summary information
    extractm <- parameters::n_factors(items,
                                      type = "FA",
                                      rotation = rotation,
                                      package = c("psych", "nFactors"),
                                      cor = cor.lv,
                                      safe = TRUE)

    # the consensus for number of factors (ties go to smallest n)
    nf <- attr(extractm, "n")

    #### Step 4 ####
    ## running efa via lavaan
    unrotated <- semTools::efaUnrotate(data = items,
                                       nf = nf,
                                       start = FALSE,
                                       ordered = ordered,
                                       missing = missing,
                                       parameterization = "theta",
                                       ...)
    # parameterization only used with ordered items, ignored otherwise;
    # "theta" seems to work better than "delta"

    ## will probably need some tryCatch statements here

    # list of unrotated factor loadings
    efa.loadings <- list(get_std_loadings(unrotated, type = "std.all"))


  } else if(efa.method == "lrt") {

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
