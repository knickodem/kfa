#' Conducts k fold cross-validation for factor analysis.
#'
#' The function splits the data into k folds. For each fold,
#' EFAs are run on the training data and the simple structure each model
#' is transformed into \code{lavaan}-compatible CFA syntax. The CFAs are then run
#' on the test data with model comparison results summarized in a rmarkdown
#' produced report.
#'
#' @param variables dataframe of variables to factor analyze
#' @param k integer; number of folds in which to split the data.
#' Default is NULL which determines k via power analysis.
#' The range for k is currently set to 2 to 10.
#' @param rmsea0 numeric; RMSEA under the null hypothesis for the power analysis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis for the power analysis.
#' @param m integer; maximum number of factors to extract. Default is 4 items per factor.
#' @param rotation character (case-sensitive); any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package.
#' Default is "oblimin".
#' @param ordered logical; Should items be treated as ordinal and the
#' polychoric correlations used in the factor analysis? When \code{FALSE} (default)
#' the Pearson correlation matrix is used. A character vector of item names is
#' also accepted to prompt estimation of the polychoric correlation matrix.
#' @param estimator if \code{ordered} = FALSE, the default is "ML". If
#' \code{ordered} = TRUE, the default is "DWLS". See \code{lavaan::lavaan} for other options.
#' @param missing default is "listwise". See \code{lavaan::lavaan} for other options.
#' @param ... arguments to pass to \code{lavaan}. These may include ...
#'
#' @details
#' Deciding an appropriate *m* can be difficult, but is consequential for both the
#' possible factor structures to examine and the computation time. To assist, researchers
#' can use \code{\link[parameters]{n_factors}} in the \code{parameters} package.

kfold_fa <- function(variables,
                     k = NULL, rmsea0 = .05, rmseaA = .08,
                     m = floor(ncol(items) / 4), rotation = "oblimin",
                     ordered = FALSE, estimator = NULL, missing = "listwise", ...){

  # The ordered = TRUE functionality in lavaan is not currently equivalent to listing
  # all items, so need to do it manually since I want this functionality for our users
  if(ordered == TRUE){
    ordered <- names(variables)
    if(is.null(estimator)){
      estimator <- "DWLS"
    }
  } else if(ordered == FALSE){
    ordered <- NULL
    if(is.null(estimator)){
      estimator <- "ML"
    }
  }

  if(is.null(k)){

    ## determine number of folds based on power analysis
    k <- findk(p = variables, m = m, rmsea0 = rmsea0, rmseaA = rmseaA)
  }

  ## create folds
  # returns list of row numbers for each training fold (i.e., the efa sample)
  # contents y doesn't matter, just needs to be nrow(variables) in length
  testfolds <- caret::createFolds(y = 1:nrow(variables),
                                  k = k, list = TRUE,
                                  returnTrain = FALSE)

  kfa <- vector(mode = "list", length = k)
  for(fold in 1:k){

    ## run EFA - returns list of cfa syntax for 1:m factor models
    efa <- k_efa(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
                 m = m,
                 rotation = rotation,
                 ordered = ordered,
                 estimator = estimator,
                 missing = missing,
                 ...)

    # ## removal of underidentified cfa syntax (syntax == "")
    # efa <- efa[lapply(efa, nchar) > 0]

    ## calculate and extract sample statistics for test sample
    sampstats <- lavaan::lavCor(variables[testfolds[[fold]], ],
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


    ## run CFAs
    cfa <- vector(mode = "list", length = length(efa))

    for(c in 1:length(efa)){

      if(nchar(efa[[c]]) > 0){

        fit <- lavaan::cfa(model = efa[[c]],
                           sample.cov = sample.cov,
                           sample.nobs = sample.nobs,
                           sample.mean = sample.mean,
                           sample.th = sample.th,
                           WLS.V = WLS.V,
                           NACOV = NACOV,
                           estimator = estimator,
                           parameterization = "delta")

        cfa[[c]] <- fit

      } else {

        cfa[[c]] <- NULL
      }

    }

    kfa[[fold]] <- cfa

  }

  return(kfa)

}
