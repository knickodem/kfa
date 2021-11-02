#' Convert to lavaan compatible factor analysis syntax
#'
#' \code{write_efa} converts variable names to exploratory factor analysis syntax whereas
#' \code{efa_efa_syntax} uses the loadings matrix (presumably from an EFA) to generate confirmatory factory analysis syntax.
#'
#' @param nf integer; number of factors
#' @param vnames character vector; names of variables to include in the efa
#'
#' @return character. Use \code{cat()} to best examine the returned syntax.
#'
#' @examples
#' vnames <- paste("x", 1:10)
#' write_efa(nf = 2, vnames = vnames)
#'
#' @export

write_efa <- function(nf, vnames){

  syntax <- character(0)
  for (f in seq_along(vnames)) {
    syntax <- c(syntax, paste0("f", f, " =~ ", paste(vnames[f:length(vnames)], collapse = " + "), "\n"))
    if (f == nf) break
  }
  syntax
}




#' @rdname write_efa
#' @param loadings matrix of factor loadings
#' @param simple logical; Should the simple structure be returned (default)?
#' If \code{FALSE}, items can cross-load on multiple factors.
#' @param threshold numeric between 0 and 1 indicating the minimum (absolute) value
#' of the loading for an item on a factor.
#' @param single.item character indicating how single-item factors should be treated.
#' Use \code{"keep"} to keep them in the model when generating the CFA syntax, \code{"drop"}
#' to remove them, or \code{"none"} indicating the CFA syntax should not be generated for
#' this model and \code{""} will be returned.
#'
#' @export

efa_cfa_syntax <- function(loadings, simple = TRUE, threshold = NA,
                           single.item = c("keep", "drop", "none")){

  if(simple == FALSE & is.na(threshold)){
    stop("threshold must be supplied when simple = FALSE")
  }

  # item and factor names
  if(is.null(dimnames(loadings))){

    vnames <- paste0("v", 1:dim(loadings)[[1]]) # variable
    fnames <- paste0("f", 1:dim(loadings)[[2]]) # factor

  } else {

    vnames <- dimnames(loadings)[[1]] # variable
    fnames <- dimnames(loadings)[[2]] # factor
  }

  if(simple == TRUE){
    # largest (absolute) loading for each item
    maxload <- apply(abs(loadings), 1, max)
  } else {
    maxload <- c(rep(NA, length(vnames)))
  }

  # obtaining simple structure matrix with NAs elsewhere
  loadings.max <- loadings
  for(v in 1:length(vnames)){

    thresh <- max(maxload[[v]], threshold, na.rm = TRUE)
    loadings.max[v, ][abs(loadings.max[v, ]) < thresh] <- NA
  }

  # returns vector with each element being the lavaan syntax identifying the factor
  cfa.syntax <- c()
  for(fn in 1:length(fnames)){
    cfa.syntax <- c(cfa.syntax,
                    paste0(fnames[[fn]], " =~ ",
                           paste(vnames[!is.na(loadings.max[,fn])],
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

  } else if(single.item == "none"){

    # check if they exist
    if(all(nchar(cfa.syntax) - nchar(gsub("+", "", cfa.syntax,fixed = TRUE)) > 0) == TRUE){

      cfa.syntax <- paste(cfa.syntax, collapse = "\n")

    } else {
      cfa.syntax <- ""
    }
  }

  return(cfa.syntax)

}


#' @rdname efa_cfa_syntax
#' @param nf integer; number of factors
#' @param vnames character vector; names of variables to include in the efa
#' @export

write_efa <- function(nf, vnames){

  syntax <- character(0)
  for (f in seq_along(vnames)) {
    syntax <- c(syntax, paste0("f", f, " =~ ", paste(vnames[f:length(vnames)], collapse = " + "), "\n"))
    if (f == nf) break
  }
  syntax
}

#' Unique factor structures
#'
#' Extract unique factor structures across the k-folds
#'
#' @param models An object returned from \code{\link[kfa]{kfa}}
#' @param which Should the unique structures be extracted from the "cfa" \code{lavaan} or "efa" syntax?
#'
#' @return \code{lavaan} syntax specifying the unique structures within each factor model in a \code{data.frame} when \code{which = "cfa"} or, when \code{which = "efa"}, a \code{list} containing the structure and the folds where the structure was identified

model_structure <- function(models, which = "cfa"){

  if(which == "cfa"){
    syntax <- models$cfa.syntax
  } else if(which == "efa"){
    syntax <- models$efas
  }

  k <- length(syntax)

  if(which == "efa"){
    m <- max(unlist(lapply(syntax, length)))
    kstructures <- vector("list", length = m)

    # currently assumes 1-factor structure exists and is the same over folds
    kstructures[[1]][[1]] <- list(structure = syntax[[1]][[1]], folds = 1:k)
    for(n in 2:m){
      structures <- vector("list", length = k)
      for(f in 1:k){
        structures[[f]] <- syntax[[f]][[n]]
      }
      kstructures[[n]] <- match_structure(structures)
    }
  } else if(which == "cfa"){

    structures <- Reduce(rbind,
                         lapply(1:k,function(x) data.frame(model = names(syntax[[x]]),
                                                           structure = unlist(syntax[[x]]))))
    kstructures <- unique(structures[structures$structure != "",])
    # there is probably a more efficient way to add folds
    folds <- as.data.frame(table(structures[structures$structure != "",]$structure))
    names(folds) <- c("structure", "folds")

    kstructures <- merge(kstructures, folds, by = "structure", all.x = TRUE, sort = FALSE)
    kstructures[,c(2,1,3)]
    kstructures$model <- factor(kstructures$model, levels = names(syntax[[1]]))
    kstructures <- kstructures[order(kstructures$model),]

    # row.names(kstructures) <- NULL
  }

  return(kstructures)
}

#' Match factor structures
#'
#' Internal function in model structures
#'
#' @param structures list of \code{lavaan} syntax
#'
#' @return For each unique structure, a \code{list} containing \code{lavaan} syntax specifying the factor structure and the folds where the structure was identified

match_structure <- function(structures){

  us <- unique(unlist(structures, use.names = FALSE))

  slist <- vector("list", length = length(us))
  # character vector of n factor elements with the variables for each factor as the elements
  fvecs <- strsplit(gsub("f[0-9+] =~ ", "", us), "\n") # currently assumes our factor naming convention of f[0-9]+ from write_efa
  for(u in seq_along(us)){
    if(u == 1){
      slist[[u]] <- list(structure = us[[u]],
                         folds = which(unlist(lapply(structures, function(x) x == us[[u]]))))
    } else {
      for(s in seq_along(slist[lengths(slist) != 0])){ # compares current structure (u) to all previous structures saved in slist
        if(identical(sort(fvecs[[s]]), sort(fvecs[[u]]))){ # if identical, combine folds, else save as new structure
          slist[[s]]$folds <- sort(c(slist[[s]]$folds, which(unlist(lapply(structures, function(x) x == us[[u]])))))
          slist[[u]] <- NULL
        } else{
          slist[[u]] <- list(structure = us[[u]],
                             folds = which(unlist(lapply(structures, function(x) x == us[[u]]))))
        }
      }
    }
  }
  slist <- slist[lengths(slist) != 0] # remove any NULL elements

  return(slist)
}
