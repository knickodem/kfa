#' Available Fit Indices
#'
#' Shows the fit indices available from \code{\link[kfa]{kfa}} object to report in \code{\link[kfa]{kfa_report}}
#'
#' @param models an object returned from \code{\link[kfa]{kfa}}
#'
#' @return character vector of index names
#'
#' @examples
#' data(example.kfa)
#' index_available(example.kfa)
#'
#' @export

index_available <- function(models){
  if(class(models) == "kfa"){
    cfas <- models$cfas
  } else {
    stop("models must be of class 'kfa'.")
  }
  # c("gfi","agfi","pgfi") throw error when missing = "fiml" (maybe other times too)
  # they are not included in the "default" selections though; see source code for lavaan::fitmeasures
  all <- tryCatch(expr = lavaan::fitmeasures(cfas[[1]][[1]]), # assumes 1-factor model in first fold is valid
                  error = function(e) return(lavaan::fitmeasures(cfas[[1]][[1]], fit.measures = "default")))
  avlbl <- all[!is.na(all)]
  return(names(avlbl))
}

#' Extract model fit
#'
#' Model fit indices extracted from k-folds
#'
#' @param models an object returned from \code{\link[kfa]{kfa}}
#' @param index character; one or more fit indices to summarize in the report. Use \code{\link[kfa]{index_available}} to see choices.
#' Chi-square value and degrees of freedom are always reported. Default is CFI and RMSEA (naive, scaled, or robust version depends on estimator used in \code{models}).
#' @param by.fold Should each element in the returned lists be a fold (default) or a factor model?
#'
#' @return \code{list} of data.frames with average model fit for each factor model
#'
#' @examples
#' data(example.kfa)
#'
#' # customize fit indices to report
#' k_model_fit(example.kfa, index = c("chisq", "cfi", "rmsea", "srmr"))
#'
#' # organize results by factor model rather than by fold
#' k_model_fit(example.kfa, by.fold = FALSE)
#'
#' @export

k_model_fit <- function(models, index = "default", by.fold = TRUE){

  if(class(models) == "kfa"){
    cfas <- models$cfas
  } else {
    stop("models must be of class 'kfa'.")
  }
  k <- length(cfas) # number of folds
  m <- max(unlist(lapply(cfas, length))) # number of models per fold
  all.index <- index_available(models)
  if("default" %in% index){
    if(sum(grepl("robust", all.index)) > 0){
      index <- c("chisq.scaled", "df.scaled", "cfi.robust", "rmsea.robust")
    } else if(sum(grepl("scaled", all.index)) > 0){
      index <- c("chisq.scaled", "df.scaled", "cfi.scaled", "rmsea.scaled")
    } else{
      index <- c("chisq", "df", "cfi", "rmsea")
    }
  } else{
    # if a scaled or robust index is requested, report scaled chisq and df
    csqdf <- if(sum(grepl("robust|scaled", index)) > 0) c("chisq.scaled", "df.scaled") else c("chisq", "df")
    index <- c(csqdf, index[!index %in% c("chisq", "df", "chisq.scaled", "df.scaled")]) # ensuring chisq and df are 1) in index and 2) listed first
    if(all(index %in% all.index) == FALSE){
      stop("one or more 'index' values not available from 'models'. Use index_available() to view choices.")
    }
  }

  ## extract fit for every model in each fold
  fits <- vector("list", length = k)
  for(f in 1:k){
    fits[[f]] <- lapply(cfas[[f]], function(x) {
      if(lavaan::lavInspect(x, "converged")){  # can only extract fit from models that converged
        lavaan::fitmeasures(x, index)
      }
    })
    fits[[f]] <- fits[[f]][lengths(fits[[f]]) != 0] # dropping NULL elements (models that did not converge)
  }

  model.names <- lapply(fits, names) # list with unique set of names for each fold

  ## organize output by folds
  kfits <- vector("list", length = k)
  for(f in 1:k){
    if(length(fits[[f]]) == 1){
      fbind <- as.data.frame(matrix(data = fits[[f]][[1]], nrow = 1, dimnames = list(NULL, index)))
    } else{
      fbind <- as.data.frame(Reduce(rbind, fits[[f]]))
    }
    fits.df <- cbind(data.frame(model = model.names[[f]]),
                     fbind)
    row.names(fits.df) <- NULL
    kfits[[f]] <- fits.df
  }
  ## organize output by model
  if(by.fold == FALSE){

    for(i in 1:length(kfits)){ # Add column for fold
      kfits[[i]] <- cbind(fold = i, kfits[[i]])
    }
    mfits <- Reduce(rbind, kfits) # bind folds into single dataframe
    # create separate list for each unique model
    mods <- models$model.names
    kfits <- vector("list", length(mods))
    for(i in 1:length(mods)){

      temp <- mfits[mfits$model == mods[[i]],]
      temp$model <- NULL
      kfits[[i]] <- temp

    }
    names(kfits) <- mods
  }
  return(kfits)
}

#' Summary table of model fit
#'
#' Summary table of model fit aggregated over k-folds
#'
#' @param kfits an object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = TRUE}
#' @param index character; one or more fit indices to summarize. Indices
#' must be present in the \code{kfits} object. Default is \code{"all"} indices present in \code{kfits}.
#' Chi-square value and degrees of freedom are always reported.
#' @param digits integer; number of decimal places to display in the report
#'
#' @return \code{data.frame} of aggregated model fit statistics
#'
#' @examples
#' data(example.kfa)
#' fits <- k_model_fit(example.kfa, by.fold = TRUE)
#' agg_model_fit(fits)
#'
#' @export

agg_model_fit <- function(kfits, index = "all", digits = 2){

  if("all" %in% index){
    index <- names(kfits[[1]])[!(names(kfits[[1]]) %in% c("model"))]
  } else if(all(index %in% names(kfits[[1]])) == FALSE){
    stop("'index' must be fit measure(s) present in 'kfits'")
  }

  ## if robust or scaled fit index requested, report scaled chisq and df, otherwise naive
  if(sum(grepl("robust|scaled", index)) > 0){

    chisq <- "chisq.scaled"
    df <- "df.scaled"

  } else{
    chisq <- "chisq"
    df <- "df"
  }
  index <- index[!index %in% c("chisq", "df", "chisq.scaled", "df.scaled")]

  bdf <- Reduce(rbind, kfits)
  degf <- tapply(bdf[[df]], bdf$model, mean)
  fit <- data.frame(model = names(degf), # names will be alphabetical order unlike unique(bdf$model)
                    df = degf)
  names(fit)[[2]] <- df

  for(i in c(chisq, index)){ # need chisq to be in the loop to get mean and range

    agg <- data.frame(model = names(degf),
                      mean = tapply(bdf[[i]], bdf$model, mean),
                      range = paste(format(round(tapply(bdf[[i]], bdf$model, min), digits = digits), nsmall = digits), "-",
                                    format(round(tapply(bdf[[i]], bdf$model, max), digits = digits), nsmall = digits)))
    names(agg) <- c("model", paste(c("mean", "range"), i, sep = "."))

    # joining into single table
    fit <- merge(x = fit, y = agg, by = "model", all.x = TRUE, sort = FALSE)
  }

  return(fit)
}

#' Internal appendix function
#'
#' Adds mean row to model fit data.frame in preparation for appendix table
#'
#' @param fits an element from the list object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = FALSE}
#' @param index one or more fit indices to include in the appendix table
#' @param suffix character to append to column names
#'
#' @return \code{data.frame} of model fit by fold and fit index
#'
#' @noRd

appendix_prep <- function(fits, index, suffix){

  # Note. 'index' value is determined in get_appendix
  fits <- fits[c("fold", index)]
  fits <- rbind(fits, cbind(data.frame(fold = "Mean", data.frame(t(colMeans(fits[-1]))))))
  names(fits) <- c("fold", paste(index, suffix, sep = "."))
  return(fits)
}

#' Appendix of model fit
#'
#' Comprehensive model fit table
#'
#' @param mfits an object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = FALSE}
#' @param index one or more fit indices to include in the appendix table. Default is \code{"all"} indices present in \code{mfits}.
#'
#' @return \code{data.frame} of model fit by fold, factor model, and fit index
#'
#' @noRd

get_appendix <- function(mfits, index = "all"){

  k <- max(unlist(lapply(mfits, nrow))) # number of folds

  # # If need to identify type of chisq and df
  # chisq <- names(mfits[[1]])[grepl("chisq", names(mfits[[1]]))] # naive or scaled?
  # df <- names(mfits[[1]])[grepl("df", names(mfits[[1]]))]

  #Note. get_appendix is currently internal, so "all" %in% index is always TRUE
  if("all" %in% index){
    index <- names(mfits[[1]])[!(names(mfits[[1]]) %in% c("fold"))]
  } else if(all(index %in% names(mfits[[1]])) == FALSE){
    stop("'index' must be fit measure(s) present in 'mfits'")
  }

  appendix <- mapply(appendix_prep, fits = mfits, suffix = names(mfits), MoreArgs = list(index = index), SIMPLIFY = FALSE)
  appendix.df <- appendix[[1]]
  if(length(mfits) > 1){
    for(i in 2:length(mfits)){
      appendix.df <- merge(appendix.df, appendix[[i]], by = "fold", all.x = TRUE)
    }
  }
  appendix.df$fold <- factor(appendix.df$fold, levels = c(1:k, "Mean"))
  appendix.df <- appendix.df[order(appendix.df$fold),]
  row.names(appendix.df) <- NULL

  return(appendix.df)
}
