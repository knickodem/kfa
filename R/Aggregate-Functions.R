#' Aggregated factor loadings
#'
#' The factor loadings aggregated over k-folds
#'
#' @param models An object returned from \code{\link[kfa]{kfa}}
#' @param flag threshold below which loading will be flagged
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return \code{data.frame} of mean factor loadings for each factor model and \code{vector} with count of folds with a flagged loading
#'
#' @export

agg_loadings <- function(models, flag = .30, digits = 2){

  cfas <- models$cfas
  vnames <- dimnames(lavaan::lavInspect(cfas[[1]][[1]], "sampstat")$cov)[[1]] # variable names
  mnames <- models$model.names # model names
  m <- length(mnames) #max(unlist(lapply(cfas, length))) # maximum number of models per fold
  k <- length(cfas) # number of folds

  klambdas <- vector("list", m)  # factor loadings across folds
  names(klambdas) <- mnames
  lflag <- vector("integer", m)  # low loading flag
  names(lflag) <- mnames
  hflag <- vector("integer", m)  # heywood case flag
  names(hflag) <- mnames
  for(n in mnames){
    lambdas <- vector("list", k)
    thetas <- vector("list", k)
    load.flag <- vector("integer", k)
    hey.flag <- vector("integer", k)
    for(f in 1:k){
      if(n %in% names(cfas[[f]])){
      ## gather standardized loadings
      lambdas[[f]] <- subset(lavaan::standardizedSolution(cfas[[f]][[n]], "std.lv", se = FALSE),
                      op == "=~")

      # flag for model summary table
      load.flag[[f]] <- sum(lambdas[[f]]$est.std < flag)

      ## gather residual variances
      thetas[[f]] <- subset(lavaan::parameterestimates(cfas[[f]][[n]], se = FALSE),
                       op == "~~" & lhs %in% vnames & lhs == rhs)

      # flag for model summary table
      hey.flag[[f]] <- sum(thetas[[f]]$est < 0)
      }
    }
    lambdas <- do.call("rbind", lambdas)
    thetas <- do.call("rbind", thetas)


    klambdas[[n]] <- data.frame(variable = vnames,
                                mean = tapply(lambdas$est.std, lambdas$rhs, mean),
                                range = paste(format(round(tapply(lambdas$est.std, lambdas$rhs, min), digits = digits), nsmall = digits), "-",
                                                format(round(tapply(lambdas$est.std, lambdas$rhs, max), digits = digits), nsmall = digits)),
                                `loading flag` = tapply(lambdas$est.std, lambdas$rhs, function(x) sum(x < flag)),
                                `heywood flag` = tapply(thetas$est, thetas$rhs, function(x) sum(x < 0)),
                                check.names = FALSE)

    ## count of folds with a loading under flag threshold
    lflag[[n]] <- sum(load.flag > 0)
    hflag[[n]] <- sum(hey.flag > 0)
  }

  return(list(loadings = klambdas,
              flag = lflag,
              heywood = hflag))
}


#' Aggregated factor correlations
#'
#' The factor correlations aggregated over k-folds
#'
#' @param models An object returned from \code{\link[kfa]{kfa}}
#' @param flag threshold above which a factor correlation will be flagged
#' @param type currently ignored; \code{"factor"} (default) or \code{"observed"} variable correlations
#'
#' @return a \code{data.frame} of mean factor correlations for each factor model and \code{vector} with count of folds with a flagged correlation
#'
#' @export

agg_cors <- function(models, flag = .90, type = "factor"){

  cfas <- models$cfas
  mnames <- models$model.names # model names
  m <- length(mnames)
  k <- length(cfas)


  kcorrs <- vector("list", m)
  kflag <- vector("integer", m)
  # Currently assumes the first element is a 1 factor model; need a more robust check
  kcorrs[[1]] <- NULL
  kflag[[1]] <- NA
  for(n in 2:m){
    cor.lv <- vector("list", k) # latent variable correlation matrix
    cor.flag <- vector("list", k) # count of correlations above flag threshold
    for(f in 1:k){
      if(mnames[[n]] %in% names(cfas[[f]])){
        cor.lv[[f]] <- lavaan::lavInspect(cfas[[f]][[mnames[[n]]]], "cor.lv")
        cor.flag[[f]] <- rowSums(cor.lv[[f]] > flag) - 1 # minus diagonal which will always be TRUE
      }
    }

    ## count of folds with a correlation over flag threshold
    fc <- unlist(cor.flag)
    cflag <- tapply(fc, names(fc), function(x) sum(x > 0)) # for each factor
    kflag[[n]] <- sum(unlist(lapply(cor.flag, sum)) > 0)      # for the model

    ## mean correlation across folds
    cor.lv <- cor.lv[lengths(cor.lv) != 0] # removes NULL elements for folds were model was not run
    aggcorrs <- Reduce(`+`, lapply(cor.lv, r2z)) / length(cor.lv) # pool results after Fisher's r to z
    aggcorrs <- z2r(aggcorrs)
    diag(aggcorrs) <- 1       # change diagonals from NaN to 1
    aggcorrs[upper.tri(aggcorrs, diag = FALSE)] <- NA
    aggcorrs <- cbind(data.frame(rn = row.names(aggcorrs)),
                      aggcorrs, data.frame(flag = cflag))
    kcorrs[[n]] <- aggcorrs

  }
  names(kcorrs) <- mnames
  names(kflag) <- mnames

 return(list(correlations = kcorrs,
             flag = kflag))
}

#' Fisher's r to z transformation
#'
#' @param r correlation
#'
#' @return z-score

r2z <- function(r){
  0.5 * log((1 + r)/(1 - r))
}

#' Fisher's z to r transformation
#'
#' @param z z-score
#'
#' @return correlation

z2r <- function(z){
  (exp(2 * z) - 1)/(1 + exp(2 * z))
}

#' Aggregated scale reliabilities
#'
#' The factor reliabilities aggregated over k-folds
#'
#' @param models An object returned from \code{\link[kfa]{kfa}}
#' @param flag threshold below which reliability will be flagged
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return a \code{data.frame} of mean factor (scale) reliabilities for each factor model and \code{vector} with count of folds with a flagged reliability
#'
#' @export

agg_rels <- function(models, flag = .60, digits = 2){

  cfas <- models$cfas
  mnames <- models$model.names # model names
  m <- length(mnames) #max(unlist(lapply(cfas, length))) # maximum number of models per fold
  k <- length(cfas) # number of folds

  # semTools::reliability does not currently calculate alpha from summary stats with categorical indicators
  what <- if(lavaan::lavInspect(cfas[[1]][[1]], "categorical")) "omega3" else c("omega3", "alpha")

  krels <- vector("list", m)
  kflag <- vector("integer", m)
  for(n in 1:m){
    rels <- vector("list", k)
    rel.flag <- vector("integer", k)
    for(f in 1:k){
      if(mnames[[n]] %in% names(cfas[[f]])){
        rels[[f]] <- t(suppressMessages(semTools::reliability(cfas[[f]][[mnames[[n]]]], what = what)))
        rel.flag[[f]] <- sum(rels[[f]][,"omega3"] < flag) # flag based on omega, not alpha
      }
    }

    ## mean reliability across folds
    aos <- do.call("rbind", rels)
    if(n == 1){
      fn <- "f1"
      fnames <- rep(fn, nrow(aos))
    } else{
      rels <- rels[lengths(rels) != 0] # removes NULL elements for folds were model was not run
      fn <- dimnames(rels[[1]])[[1]]  # grabs unique factor names from first fold; should be the same in all folds
      fnames <- dimnames(aos)[[1]] # should be the equivalent of rep(fn, nrow(aos))
    }

    rdf <- data.frame(factor = fn)
    for(w in 1:length(what)){
      test <- data.frame(mean = tapply(aos[, w], fnames, mean),
                         range = paste(format(round(tapply(aos[, w], fnames, min), digits = digits), nsmall = digits), "-",
                                       format(round(tapply(aos[, w], fnames, max), digits = digits), nsmall = digits)),
                         flag = tapply(aos[, w], fnames, function(x) sum(x < flag)))
      names(test) <- paste(what[[w]], names(test), sep = ".")
      rdf <- cbind(rdf, test)

    }
    krels[[n]] <- rdf

    ## count of folds with a reliabilities below flag threshold
    kflag[[n]] <- sum(rel.flag > 0)
  }
  names(krels) <- mnames
  names(kflag) <- mnames

  return(list(reliabilities = krels,
              flag = kflag))
}

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
#' @export

k_model_fit <- function(models, index = c("chisq", "cfi", "rmsea"), by.fold = TRUE){

  cfas <- models$cfas
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
      fits.df <- cbind(data.frame(model = model.names[[f]]),
                       as.data.frame(Reduce(rbind, fits[[f]])))
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
#' @param index One or more fit indices to summarize in the report. The degrees of freedom are always reported. Default are "chisq", "cfi", and "rmsea".
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return \code{data.frame} of aggregated model fit statistics
#'
#' @export

agg_model_fit <- function(kfits, index = c("chisq", "cfi", "rmsea"), digits = 2){

  # if(!index %in% names(kfits[[1]])){
  #   stop("index must be fit measure(s) extracted in kfits")
  # }

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
#' @param index One or more fit indices to include in the appendix table. Default is \code{"all"} indices present in \code{fits}. The degrees of freedom are always reported.
#' @param suffix character to append to column names
#'
#' @return \code{data.frame} of model fit by fold, factor model, and fit index

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

get_appendix <- function(mfits, index = "all"){

  k <- max(unlist(lapply(mfits, nrow))) # number of folds

  if(length(index) == 1){
    if(index == "all"){
      index <- names(mfits[[1]])[!(names(mfits[[1]]) %in% c("fold", "df"))]
    }
  }

  appendix <- mapply(appendix_prep, fits = mfits, suffix = names(mfits), MoreArgs = list(index = index), SIMPLIFY = FALSE)
  appendix.df <- appendix[[1]]
  for(i in 2:length(mfits)){
    appendix.df <- merge(appendix.df, appendix[[i]], by = "fold", all.x = TRUE)
  }
  appendix.df$fold <- factor(appendix.df$fold, levels = c(1:k, "Mean"))
  appendix.df <- appendix.df[order(appendix.df$fold),]
  row.names(appendix.df) <- NULL

  return(appendix.df)
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
  for(u in seq_along(us)){

    folds <- which(unlist(lapply(structures, function(x) x == us[[u]])))

    slist[[u]] <- list(structure = us[[u]],
                       folds = which(unlist(lapply(structures, function(x) x == us[[u]]))))
  }

  return(slist)
}


#' Flag model problems
#'
#' Internal function to create table of flag counts
#'
#' @param models An object returned from \code{\link[kfa]{kfa}}
#' @param strux An object returned from \code{\link[kfa]{model_structures}} when \code{which = "cfa"}
#' @param loads An object returned from \code{\link[kfa]{agg_loadings}}
#' @param cors An object returned from \code{\link[kfa]{agg_cors}}
#' @param rels An object returned from \code{\link[kfa]{agg_rels}}
#'
#' @return a \code{data.frame}

model_flags <- function(models, strux, loads, cors, rels){

  # Multiple structures from EFA
  strux <- strux[c("model", "folds")]
  names(strux) <- c("model", "mode structure")

  # flags from CFA models
  cfas <- models$cfas
  k <- length(cfas)
  m <- max(unlist(lapply(cfas, length)))

  ## Flagging non-convergence and non-positive definite matrix warnings
  cnvgd <- vector("list", k)
  hey <- vector("list", k)
  for(f in 1:k){

    ## convergence status of each model
    cnvgd[[f]] <- lapply(cfas[[f]], lavaan::lavInspect, "converged")
    ## presence of non-positive definite matrix (and heywood cases) in each model
    hey[[f]] <- lapply(cfas[[f]], lavaan::lavInspect, "post.check")

  }

  # flagged if either indicates an improper solution
  temp <- c(unlist(cnvgd) == FALSE, unlist(hey) == FALSE)
  improper <-tapply(temp, names(temp), sum)

  ## joining flags into data.frame
  flags <- data.frame(model = models$model.names,  # assumes first fold contains all models in the same order as other folds
                      `improper solution` = improper,
                      `heywood item` = loads$heywood,
                      `low loading` = loads$flag,
                      `high factor correlation` = cors$flag,
                      `low scale reliability` = rels$flag,
                      check.names = FALSE)

  flags <- merge(strux, flags, by = "model", all.x = FALSE, all.y = TRUE, sort = FALSE)

  return(flags)
}

