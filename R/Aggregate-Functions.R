#' Aggregated factor loadings
#'
#' The factor loadings aggregated over k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#'
#' @return \code{data.frame} of mean factor loadings for each factor model

agg_loadings <- function(kfa){

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))
  vnames <- dimnames(lavaan::lavInspect(kfa[[1]][[1]], "sampstat")$cov)[[1]]

  klambdas <- vector("list", m)
  for(n in 1:m){
    lambdas <- data.frame()
    for(f in 1:k){
      loads <- subset(lavaan::standardizedSolution(kfa[[f]][[n]], "std.lv",
                                                   se = FALSE, zstat = FALSE,
                                                   pvalue = FALSE, ci = FALSE),
                      op == "=~")
      lambdas <- rbind(lambdas, loads)
    }

    klambdas[[n]] <- data.frame(variable = vnames,
                                mean = tapply(lambdas$est.std, lambdas$rhs, mean),
                                min = tapply(lambdas$est.std, lambdas$rhs, min),
                                max = tapply(lambdas$est.std, lambdas$rhs, max))
  }
  return(klambdas)
}


#' Aggregated factor correlations
#'
#' The factor correlations aggregated over k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#'
#' @return \code{data.frame} of factor correlations for each factor model

agg_fac_cor <- function(kfa){

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))

  kcorrs <- vector("list", maxfac)
  kcorrs[[1]] <- NULL  # Currently assumes the first element is a 1 factor model; need a more robust check
  for(n in 2:m){
    cor.lv <- vector("list", k)
    for(f in 1:k){
      cor.lv[[f]] <- lavaan::lavInspect(kfa[[f]][[n]], "cor.lv")
    }

    aggcorrs <- Reduce(`+`, cor.lv) / length(cor.lv)
    aggcorrs[upper.tri(aggcorrs, diag = FALSE)] <- NA
    aggcorrs <- cbind(data.frame(rn = row.names(aggcorrs)), aggcorrs)
    kcorrs[[n]] <- aggcorrs

  }
 return(kcorrs)
}

#' Aggregated scale reliabilities
#'
#' The factor reliabilities aggregated over k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#'
#' @return \code{data.frame} of factor reliabilities for each factor model

agg_reliability <- function(kfa){

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))

  krels <- vector("list", m)
  for(n in 1:m){
    rels <- vector("list", k)
    for(f in 1:k){
      rels[[f]] <- semTools::reliability(kfa[[f]][[n]])[c(1,4),]
    }
    krels[[n]] <- Reduce(`+`, rels) / length(rels)
    if(n == 1){
      krels[[n]] <- as.data.frame(krels[[n]])
      names(krels[[n]]) <- "f1"
    }
    krels[[n]] <- cbind(data.frame(type = c("alpha", "omega_h")), krels[[n]])
  }
  return(krels)
}

#' Extract model fit
#'
#' Model fit indices extracted from k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#' @param index One or more fit indices to summarize in the report. The degrees of freedom are always reported. Default are "chisq", "cfi", and "rmsea".
#' @param by.fold Should each element in the returned lists be a fold (default) or a factor model?

#' @return \code{list} of data.frames with average model fit for each factor model

k_model_fit <- function(kfa, index = c("chisq", "cfi", "rmsea"), by.fold = TRUE){

  k <- length(kfa) # number of folds
  m <- max(unlist(lapply(kfa, length))) # number of models per fold
  index <- ifelse(grepl("df", index), index, c("df", index))

  ## extract fit for every model in each fold
  fits <- vector("list", length = k)
  for(f in 1:k){
    fits[[f]] <- lapply(kfa[[f]], function(x) {
      lavaan::fitmeasures(x, index)
    })
  }

  ## organize output by folds or by model
  if(by.fold == TRUE){

    kfits <- vector("list", length = k)
    for(f in 1:k){
      fits.df <- cbind(data.frame(factors = as.character(1:m)),
                       as.data.frame(Reduce(rbind, fits[[f]])))
      row.names(fits.df) <- NULL
      kfits[[f]] <- fits.df
    }
  } else {

    kfits <- vector("list", length = m)
    for(n in 1:m){
      fits.df <- cbind(data.frame(fold = as.character(1:k)),
                       as.data.frame(Reduce(rbind, lapply(fits, function(x) x[[n]]))))
      row.names(fits.df) <- NULL
      kfits[[n]] <- fits.df
    }
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

agg_model_fit <- function(kfits, index = c("chisq", "cfi", "rmsea"), digits = 2){

  # if(!index %in% names(kfits[[1]])){
  #   stop("index must be fit measure(s) extracted in kfits")
  # }

  index <- index[index != "df"] # df is added automatically so don't need to loop through it
  maxfac <- max(unlist(lapply(kfits, nrow)))

  bdf <- as.data.frame(Reduce(rbind, kfits))

  fit <- data.frame(factors = 1:maxfac,
                    df = tapply(bdf[["df"]], bdf$factors, mean))
  for(i in index){

    agg <- data.frame(factors = 1:maxfac,
                      mean = tapply(bdf[[i]], bdf$factors, mean),
                      range = paste(format(round(tapply(bdf[[i]], bdf$factors, min), digits = digits), nsmall = digits), "-",
                                    format(round(tapply(bdf[[i]], bdf$factors, max), digits = digits), nsmall = digits)))
    # hist = tapply(bdf[[index]], bdf$factors, function(x) skimr::skim(x)[["numeric.hist"]])
    names(agg) <- c("factors", paste(c("mean", "range"), i, sep = "."))


    # joining into single table
    fit <- merge(x = fit, y = agg, by = "factors", all.x = TRUE)
  }

  return(fit)
}

#' Model with best fit
#'
#' Summary table of best fitting model in k-folds
#'
#' @param kfits An object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = TRUE}
#' @param index One or more fit indices to summarize in the report. Default are "chisq", "cfi", and "rmsea".
#'
#' @return \code{data.frame} with fold in rows, index in columns, and factor model with best fit in the cells

best_model <- function(kfits, index = c("chisq", "cfi", "rmsea")){

  k <- length(kfa) # number of folds
  index <- index[index != "df"]

  best.df <- data.frame(fold = 1:k)
  for(i in index){

    if(i %in% c("chisq", "rmsea", "srmr")){

      best <- lapply(kfits, function(x) x[x[[i]] == min(x[[i]]),])

    } else if(i %in% c("cfi", "tli")){

      best <- lapply(kfits, function(x) x[x[[i]] == max(x[[i]]),])

    } else {stop("Not available at the moment")}

    best <- as.data.frame(Reduce(rbind, best))
    best.df <- cbind(best.df, best$factors)
  }

  names(best.df) <- c("fold", index)

  return(best.df)
}

#' Appendix of model fit
#'
#' Comprehensive model fit table
#'
#' @param mfits An object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = FALSE}
#' @param index One or more fit indices to summarize in the report. The degrees of freedom are always reported. Default are "chisq", "cfi", and "rmsea".
#'
#' @return \code{data.frame} of model fit by fold, factor model, and fit index

get_appendix <- function(mfits, index = c("chisq", "cfi", "rmsea")){

  k <- max(unlist(lapply(mfits, nrow))) # number of folds
  m <- length(mfits) # number of models per fold
  index <- index[index != "df"]

  appendix <- mapply(appendix_prep, fits = mfits, suffix = 1:m, MoreArgs = list(index = index), SIMPLIFY = FALSE)
  appendix.df <- appendix[[1]]
  for(i in 2:m){
    appendix.df <- merge(appendix.df, appendix[[i]], by = "fold")
  }
  appendix.df$fold <- factor(appendix.df$fold, levels = c(1:k, "Mean"))
  appendix.df <- appendix.df[order(appendix.df$fold),]
  row.names(appendix.df) <- NULL

  return(appendix.df)
}

appendix_prep <- function(fits, index, suffix){

  fits <- fits[c("fold", "df", index)]
  fits <- rbind(fits, cbind(data.frame(fold = "Mean", data.frame(t(colMeans(fits[-1]))))))
  names(fits) <- c("fold", paste(c("df", index), suffix, sep = "."))
  return(fits)
}

#' Unique factor structures
#'
#' Extract unique factor structures across the k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#'
#' @return A \code{list} containing \code{lavaan} syntax specifying the factor structure and the folds where the structure was identified

model_structure <- function(kfa){

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))

  kstructures <- vector("list", length = m)
  kstructures[[1]][[1]] <- list(structure = efa_cfa_syntax(lavaan::lavInspect(kfa[[1]][[1]], "est")$lambda),
                                folds = 1:k)
  for(n in 2:m){
    structures <- vector("list", length = k)
    for(f in 1:k){
      structures[[f]] <- efa_cfa_syntax(lavaan::lavInspect(kfa[[f]][[n]], "est")$lambda)
    }

    us <- unique(unlist(structures, use.names = FALSE))

    slist <- vector("list", length = length(us))
    for(u in seq_along(us)){

      folds <- which(unlist(lapply(structures, function(x) x == us[[u]])))

      slist[[u]] <- list(structure = us[[u]],
                         folds = which(unlist(lapply(structures, function(x) x == us[[u]]))))
    }

    kstructures[[n]] <- slist
  }
  return(kstructures)
}
