#' Conducts k fold cross-validation for factor analysis.
#'
#' The function splits the data into k folds. For each fold,
#' EFAs are run on the training data and the simple structure each model
#' is transformed into \code{lavaan}-compatible CFA syntax. The CFAs are then run
#' on the test data with model comparison results summarized in a rmarkdown
#' produced report.
#'
#' @param items dataframe of item responses
#' @param efa.method character; How should the EFA models be determined?
#' "sequence" sequentially runs 1 to *m* factor models.
#' "consensus" calls \code{\link[parameters]{n_factors}} which identifies the
#' optimal number of factors based the consensus of a variety of methods.
#' @param k integer; number of folds in which to split the data.
#' Default is NULL which determines k via power analysis.
#' The range for k is currently set to 2 to 10.
#' @param rmsea0 numeric; RMSEA under the null hypothesis for the power analysis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis for the power analysis.
#' @param m integer; maximum number of factors to extract. Default is 4 items per factor.
#' @param ordered logical; Should items be treated as ordinal and the
#' polychoric correlations used in the factor analysis? When FALSE (default)
#' the Pearson correlation matrix is used. A character vector of item names is
#' also accepted to prompt estimation of the polychoric correlation matrix.
#' @param ... arguments to pass to \code{lavaan}. These may include ...

kfold_fa <- function(items, efa.method, rotation = "oblimin",
                     k = NULL, rmsea0 = .05, rmseaA = .08,
                     m = floor(ncol(items) / 4), threshold = NA, # might not make threshold an argument
                     ordered = NULL, missing = "listwise", ...){

  # The ordered = TRUE functionality in lavaan is not currently equivalent to listing
  # all items, so need to do it manually since I want this functionality for our users
    if(ordered == TRUE){
      ordered <- names(items)
    } else if(ordered == FALSE){
      ordered <- NULL
    }

  if(is.null(k)){

    ## determine number of folds based on power analysis
    k <- findk(items = items, m = m, rmsea0 = rmsea0, rmseaA = rmseaA)
  }

  ## create folds
  # returns list of row numbers for each training fold (i.e., the efa sample)
  # contents y doesn't matter, just needs to be nrow(items) in length
  trainfolds <- caret::createFolds(y = 1:nrow(items),
                                   k = k, list = TRUE,
                                   returnTrain = TRUE)

  efa <- vector(mode = "list", length = k)
  for(fold in 1:k){

    ## run EFA - returns list of cfa syntax for 1:m factor models
    efa[[fold]] <- k_efa(items = items[trainfolds[[fold]], ],
                         efa.method = efa.method,
                         rotation = rotation,
                         m = m,
                         threshold = threshold,
                         ordered = ordered,
                         missing = missing,
                         ...)
  }

  ## Possible stopping point for examination of EFA results

  ## removal of underidentified cfa syntax (syntax == "")

  # for(fold in 1:k){
  #   ## run CFA
  #
  # }


return(efa)

}
