#' Conducts k-fold cross validation for factor analysis.
#'
#' The function splits the data into *k* folds. For each fold,
#' EFAs are run on the training data and the simple structure each model
#' is transformed into \code{lavaan}-compatible CFA syntax. The CFAs are then run
#' on the test data.
#'
#' @param variables a \code{data.frame} (or convertible to a \code{data.frame}) of variables to factor analyze
#' @param k an integer between 2 and 10; number of folds in which to split the data. Default is NULL which determines k via power analysis.
#' @param m integer; maximum number of factors to extract. Default is 4 items per factor.
#' @param custom.cfas a single object or named \code{list} of \code{lavaan} syntax specifying custom factor model(s).
#' @param rmsea0 numeric; RMSEA under the null hypothesis for the power analysis.
#' @param rmseaA numeric; RMSEA under the alternative hypothesis for the power analysis.
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
#' In order to be tested along with the EFA identified structures, each model supplied in \code{custom.cfas} must
#' include all \code{variables} in \code{lavaan} compatible syntax. Thus, to simultaneously test a model dropping
#' a \code{variable}, have the variable load on to a factor while constraining the loading to 0.
#'
#' Deciding an appropriate *m* can be difficult, but is consequential for both the possible factor structures to
#' examine and the computation time. The \code{\link[parameters]{n_factors}} in the \code{parameters} package
#' can assist with this decision.
#'
#' @return A two-element \code{list}:
#' \itemize{
#' \item \code{lavaan} CFA objects for each *k* fold
#' \item all factor structures identified in the EFA
#' }
#'
#' @export

kfa <- function(variables,
                k = NULL,
                m = floor(ncol(variables) / 4),
                efa.only = FALSE,
                custom.cfas = NULL,
                rmsea0 = .05, rmseaA = .08,
                rotation = "oblimin", ordered = FALSE,
                estimator = NULL, missing = "listwise", ...){

  variables <- as.data.frame(variables)

  # The ordered = TRUE functionality in lavaan is not currently equivalent to listing
  # all items, so need to do it manually since I want this functionality for our users
  if(is.character(ordered)){
    if(is.null(estimator)){
      estimator <- "DWLS"
    }
  }
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

  if(length(testfolds[[1]]) < 200){
    warning("Sample size for each test fold is ", length(testfolds[[1]]), ",\nwhich is smaller than the recommended 200 (Curran, Bollen, Chen, Paxton, & Kirby, 2003)")
  }

  if(.Platform$OS.type == "windows"){
    cluster.type <- "PSOCK"
  } else{
    cluster.type <- "FORK"
  }

  cores <- parallel::detectCores() - 1
  clusters <- parallel::makeCluster(cores, type = cluster.type)
  doParallel::registerDoParallel(clusters)

  print(paste("Using", foreach::getDoParWorkers(), "cores for parallelization."))

  ## Run EFAs and return lavaan syntax for CFAs
  efa <- foreach::foreach(fold = 1:k) %dopar% { # use %do% if we offer a parallel = FALSE option
    k_efa(variables = variables[!c(row.names(variables) %in% testfolds[[fold]]), ],
          m = m,
          rotation = rotation,
          ordered = ordered,
          estimator = estimator,
          missing = missing,
          ...)
  }

  # formatting for use in model_structure
  temp <- list(cfas = NULL,
               efas = efa)

  # identifying unique structure
  efa.structures <- model_structure(temp, which = "efa")
  names(efa.structures) <- paste0(1:m, "-factor")

  # collect most common structure for each factor model to use in CFAs
  cfa.syntax <- vector("list", length = m)
  for(n in 1:m){
    if(length(efa.structures[[n]]) == 1){ # all one factor structures should be the same
      cfa.syntax[[n]] <- efa.structures[[n]][[1]]$structure
    } else {
      # number of folds each structure was found; keep most common structure
      num.folds <- unlist(lapply(efa.structures[[n]], function(x) length(x$folds)))
      ties <- which(num.folds == max(num.folds))
      ties <- ifelse(length(ties) == 1, ties, ties[[1]]) # in case of ties, use first structure
      cfa.syntax[[n]] <- efa.structures[[n]][[ties]]$structure
    }
  }
  names(cfa.syntax) <- paste0(1:m, "-factor")

  if(!is.null(custom.cfas)){
    if(class(custom.cfas) != "list"){ # converting single object to named list
      custom.name <- deparse(substitute(custom.cfas))
      custom.cfas <- list(custom.cfas)
      names(custom.cfas) <- custom.name
    } else if(is.null(names(custom.cfas))){ # (if necessary) adding names to list
      names(custom.cfas) <- paste("custom", LETTERS[1:length(custom.cfas)])
    }
    cfa.syntax <- c(cfa.syntax, custom.cfas)
  }

  ## Run CFAs
  cfas <- foreach::foreach(fold = 1:k) %dopar% {
    k_cfa(syntax = cfa.syntax,
          variables = variables[testfolds[[fold]], ],
          ordered = ordered,
          estimator = estimator,
          missing = missing,
          ...)
  }
  output <- list(cfas = cfas,
                 cfa.syntax = cfa.syntax,
                 efa.structures = efa.structures)

  parallel::stopCluster(clusters)

  return(output)
}
