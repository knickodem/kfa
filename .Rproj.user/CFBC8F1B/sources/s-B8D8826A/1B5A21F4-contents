#' Converts EFA results into CFA syntax
#'
#' Uses the matrix of loadings from an EFA to generate \code{lavaan} compatible
#' CFA syntax.
#'
#' @param loadings matrix of factor loadings
#' @param simple logical; Should the simple structure be returned (default)?
#' If \code{FALSE}, items can cross load on multiple factors.
#' @param threshold numeric between 0 and 1 indicating the minimum (absolute) value
#' of the loading for an item on a factor.
#' @param single.item character indicating how single-item factors should be treated.
#' Use "keep" to keep them in the model when generating the CFA syntax, "drop"
#' to remove them, or "" indicating the CFA syntax should not be generated for this model.
#'
#' @return character. Use \code{cat} to best examine the returned syntax.

efa_cfa_syntax <- function(loadings, simple = TRUE, threshold = NA,
                           single.item = c("keep","drop", "")){

  if(simple == FALSE & is.na(threshold)){
    stop("threshold must be supplied when simple = FALSE")
  }

  # item and factor names
  if(is.null(dimnames(loadings))){

    inames <- paste0("i", 1:dim(loadings)[[1]]) # item
    fnames <- paste0("f", 1:dim(loadings)[[2]]) # factor

  } else {

    inames <- dimnames(loadings)[[1]] # item
    fnames <- dimnames(loadings)[[2]] # factor
  }

  if(simple == TRUE){
    # largest (absolute) loading for each item
    maxload <- apply(abs(loadings), 1, max)
  } else {
    maxload <- c(rep(NA, length(inames)))
  }

  # obtaining simple structure matrix with NAs elsewhere
  loadings.max <- loadings
  for(i in 1:length(inames)){
    thresh <- max(maxload[[i]], threshold, na.rm = TRUE)

    loadings.max[i, ][abs(loadings.max[i, ]) < thresh] <- NA

  }

  # returns vector with each element being the lavaan syntax identifying the factor
  cfa.syntax <- c()
  for(fn in 1:length(fnames)){
    cfa.syntax <- c(cfa.syntax,
                    paste0(fnames[[fn]], " =~ ",
                           paste(inames[!is.na(loadings.max[,fn])],
                                 collapse = " + ")))
  }

  # What to do with single item factors?
  if(length(single.item) > 1){
    single.item <- "keep"
  }
  if(single.item == "keep"){

    # final cfa syntax
    cfa.syntax <- paste(cfa.syntax, collapse = "\n")

  } else if(single.item == "drop"){

    # drops single item factors before collapsing to final syntax
    cfa.syntax <- cfa.syntax[nchar(cfa.syntax) - nchar(gsub("+", "", cfa.syntax,
                                                            fixed = TRUE)) > 0]
    cfa.syntax <- paste(cfa.syntax, collapse = "\n")

  } else if(single.item == ""){

    # check if they exist
    if(all(nchar(cfa.syntax) - nchar(gsub("+", "", cfa.syntax,fixed = TRUE)) > 0) == TRUE){

      cfa.syntax <- paste(cfa.syntax, collapse = "\n")

    } else {
      cfa.syntax <- ""
    }
  }

  return(cfa.syntax)

}
