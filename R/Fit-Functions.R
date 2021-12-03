#' Extract model fit
#'
#' Model fit indices extracted from k-folds
#'
#' @param models An object returned from \code{\link[kfa]{kfa}}
#' @param index One or more fit indices to summarize in the report. The degrees of freedom are always reported. Default are "chisq", "cfi", and "rmsea".
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

k_model_fit <- function(models, index = c("chisq", "cfi", "rmsea"), by.fold = TRUE){

  if(class(models) == "kfa"){
    cfas <- models$cfas
  } else {
    stop("models must be of class 'kfa'.")
  }
  k <- length(cfas) # number of folds
  m <- max(unlist(lapply(cfas, length))) # number of models per fold
  index <- if(sum(grepl("df", index)) == 0) c("df", index) else index

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
#' @param kfits An object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = TRUE}
#' @param index Character vector of one or more fit indices to summarize in the report. Indices
#' must be present in the \code{kfits} object extracted in \code{\link[kfa]{k_model_fit}}.
#' Default is \code{"all"} indices present in \code{kfits}. Degrees of freedom are always reported.
#' @param digits Integer; number of decimal places to display in the report.
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

  if(length(index) == 1){
    if(index == "all"){
      index <- names(kfits[[1]])[!(names(kfits[[1]]) %in% c("model", "df"))]
    }
  }

  if(sum(!index %in% names(kfits[[1]])) > 0){
    stop("index must be fit measure(s) extracted in kfits")
  }

  index <- index[index != "df"] # df is added automatically so don't need to loop through it
  bdf <- Reduce(rbind, kfits)
  degf <- tapply(bdf[["df"]], bdf$model, mean) # named (by model) vector of degrees of freedom
  fit <- data.frame(model = names(degf),
                    df = degf)
  for(i in index){

    agg <- data.frame(model = names(degf),
                      mean = tapply(bdf[[i]], bdf$model, mean),
                      range = paste(format(round(tapply(bdf[[i]], bdf$model, min), digits = digits), nsmall = digits), "-",
                                    format(round(tapply(bdf[[i]], bdf$model, max), digits = digits), nsmall = digits)))
    # hist = tapply(bdf[[index]], bdf$factors, function(x) skimr::skim(x)[["numeric.hist"]])
    names(agg) <- c("model", paste(c("mean", "range"), i, sep = "."))


    # joining into single table
    fit <- merge(x = fit, y = agg, by = "model", all.x = TRUE, sort = FALSE)
  }

  return(fit)
}

#' Internal appendix function
#'
#' Prepare model fit results for appendix table
#'
#' @param fits An element from the list object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = FALSE}
#' @param index One or more fit indices to include in the appendix table. Default is \code{"all"} indices present in \code{fits}.
#' Degrees of freedom are always reported.
#' @param suffix character to append to column names
#'
#' @return \code{data.frame} of model fit by fold, factor model, and fit index
#'
#' @noRd

appendix_prep <- function(fits, index, suffix){

  fits <- fits[c("fold", "df", index)]
  fits <- rbind(fits, cbind(data.frame(fold = "Mean", data.frame(t(colMeans(fits[-1]))))))
  names(fits) <- c("fold", paste(c("df", index), suffix, sep = "."))
  return(fits)
}

#' Appendix of model fit
#'
#' Comprehensive model fit table
#'
#' @param mfits An object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = FALSE}
#' @param index One or more fit indices to include in the appendix table. Default is \code{"all"} indices present in \code{mfits}. The degrees of freedom are always reported.
#'
#' @return \code{data.frame} of model fit by fold, factor model, and fit index
#'
#' @noRd

get_appendix <- function(mfits, index = "all"){

  k <- max(unlist(lapply(mfits, nrow))) # number of folds

  if(length(index) == 1){
    if(index == "all"){
      index <- names(mfits[[1]])[!(names(mfits[[1]]) %in% c("fold", "df"))]
    }
  }

  if(sum(!index %in% names(mfits[[1]])) > 0){
    stop("index must be fit measure(s) extracted in mfits")
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
