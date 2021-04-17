#' Conducts k-fold cross validation for factor analysis.
#'
#' The function splits the data into *k* folds. For each fold,
#' EFAs are run on the training data and the simple structure each model
#' is transformed into \code{lavaan}-compatible CFA syntax. The CFAs are then run
#' on the test data.
#'
#' @param variables a \code{data.frame} (or convertible to a \code{data.frame}) of variables to factor analyze
#' @param k an integer between 2 and 10; number of folds in which to split the data. Default is NULL which determines k via power analysis.
#' @param rmsea0 numeric; RMSEA under the null hypothesis for the power analysis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis for the power analysis.
#' @param m integer; maximum number of factors to extract. Default is 4 items per factor.
#' @param rotation character (case-sensitive); any rotation method listed in
#' \code{\link[GPArotation]{rotations}} in the \code{GPArotation} package. Default is "oblimin".
#' @param ordered logical; Should items be treated as ordinal and the
#' polychoric correlations used in the factor analysis? When \code{FALSE} (default)
#' the Pearson correlation matrix is used. A character vector of item names is
#' also accepted to prompt estimation of the polychoric correlation matrix.
#' @param estimator if \code{ordered = FALSE}, the default is "ML". If
#' \code{ordered = TRUE}, the default is "DWLS". See \code{\link[lavaan]{lavaan}} for other options.
#' @param missing default is "listwise". See \code{\link[lavaan]{lavaan}} for other options.
#' @param ... other arguments passed to \code{lavaan} functions. See \code{\link[lavaan]{lavOptions}}.
#'
#' @details
#' Deciding an appropriate *m* can be difficult, but is consequential for both the
#' possible factor structures to examine and the computation time.
#' The \code{\link[parameters]{n_factors}} in the \code{parameters} package can assist with this decision.
#'
#' @return A \code{list} of \code{lists} with *k* outer elements for each fold and *m* inner elements containing a \code{lavaan} object.
#'
#' @export

kfa <- function(variables,
                k = NULL, rmsea0 = .05, rmseaA = .08,
                m = floor(ncol(variables) / 4), rotation = "oblimin",
                ordered = FALSE, estimator = "default", missing = "listwise", ...){

  variables <- as.data.frame(variables)

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
    k <- findk(variables = variables, m = m, rmsea0 = rmsea0, rmseaA = rmseaA)
  }

  ## create folds
  # returns list of row numbers for each test fold (i.e., the cfa sample)
  # contents of y doesn't matter, just needs to be nrow(variables) in length
  testfolds <- caret::createFolds(y = 1:nrow(variables),
                                  k = k, list = TRUE,
                                  returnTrain = FALSE)

  if(.Platform$OS.type == "windows"){
    cluster.type <- "PSOCK"
  } else{
    cluster.type <- "FORK"
  }

  cores <- parallel::detectCores() - 1
  clusters <- parallel::makeCluster(cores, type = cluster.type)
  doParallel::registerDoParallel(clusters)

  print(paste("Using", foreach::getDoParWorkers(), "cores for parallelization."))

  ## Run EFAs and return lavaan syntax for CFAs - uses all available cores (except for windows os)
  efa <- foreach::foreach(fold = 1:k) %dopar% { # use %do% if we offer a parallel = FALSE option
    k_efa(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
          m = m,
          rotation = rotation,
          ordered = ordered,
          estimator = estimator,
          missing = missing,
          ...)
  }

  ## Run CFAs
  cfa <- foreach::foreach(fold = 1:k) %dopar% {
    k_cfa(efa = efa[[fold]],
          variables = variables[testfolds[[fold]], ],
          ordered = ordered,
          estimator = estimator,
          missing = missing,
          ...)
  }
  parallel::stopCluster(clusters)

  return(cfa)
}
