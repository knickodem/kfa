#' Aggregated factor loadings
#'
#' The factor loadings aggregated over k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#'
#' @return \code{data.frame} of mean factor loadings for each factor model

agg_loadings <- function(kfa){

  k <- length(kfa)
  maxfac <- max(unlist(lapply(kfa, length)))
  vnames <- dimnames(lavaan::lavInspect(kfa[[1]][[1]], "sampstat")$cov)[[1]]

  klambdas <- vector("list", maxfac)
  for(m in 1:maxfac){
    lambdas <- data.frame()
    for(s in 1:k){
      loads <- subset(lavaan::standardizedSolution(kfa[[s]][[m]], "std.lv",
                                                   se = FALSE, zstat = FALSE,
                                                   pvalue = FALSE, ci = FALSE),
                      op == "=~")
      # loads$factors <- m
      # loads$fold <- s
      lambdas <- rbind(lambdas, loads)
    }

    klambdas[[m]] <- data.frame(variable = vnames,
                                mean = tapply(lambdas$est.std, lambdas$rhs, mean),
                                min = tapply(lambdas$est.std, lambdas$rhs, min),
                                max = tapply(lambdas$est.std, lambdas$rhs, max))
    # row.names(agglambdas) <- NULL
    # klambdas[[m]] <- agglambdas
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

agg_f_cor <- function(kfa){

  k <- length(kfa)
  maxfac <- max(unlist(lapply(kfa, length)))

  kcorrs <- vector("list", maxfac)
  kcorrs[[1]] <- NULL  # Currently assumes the first element is a 1 factor model; need a more robust check
  for(m in 2:maxfac){
    cor.lv <- vector("list", k)
    for(s in 1:k){
      cor.lv[[s]] <- lavaan::lavInspect(kfa[[s]][[m]], "cor.lv")
    }

    aggcorrs <- Reduce(`+`, cor.lv) / length(cor.lv)
    aggcorrs[upper.tri(aggcorrs, diag = FALSE)] <- NA
    aggcorrs <- cbind(data.frame(rn = row.names(aggcorrs)), aggcorrs)
    kcorrs[[m]] <- aggcorrs

  }
 return(kcorrs)
}

#' Extract model fit
#'
#' Model fit indices extracted from k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#'
#' @return \code{data.frame} of average model fit for each factor model

k_model_fit <- function(kfa){

  k <- length(kfa)

  kfits <- vector("list", length = k)
  for(f in 1:k){

    fits <- lapply(kfa[[f]], function(x) {
      lavaan::fitmeasures(x, c("chisq", "df", "cfi",
                               "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
    })

    fits.df <- cbind(data.frame(factors = as.character(1:length(fits))),
                     as.data.frame(Reduce(rbind, fits)))
    row.names(fits.df) <- NULL

    kfits[[f]] <- fits.df
  }
  return(kfits)
}

#' Summary table of model fit
#'
#' Summary table of average model fit from the k-folds
#'
#' @param k.fits An object returned from \code{\link[kfa]{k_model_fit}}
#'
#' @details Only RMSEA is currently summarized
#'
#' @return For each factor quantity, a \code{list} containing \code{lavaan} syntax specifying the factor structure and the folds where the structure was identified

agg_model_fit <- function(k.fits, index = "rmsea"){

  if(!index %in% names(k.fits[[1]])){
    stop("index must be a fit measure extracted in k.fits")
  }
  maxfac <- max(unlist(lapply(k.fits, nrow)))

  bdf <- as.data.frame(Reduce(rbind, k.fits))
  fit <- data.frame(factors = 1:maxfac,
                       mean = tapply(bdf[[index]], bdf$factors, mean),
                       min = tapply(bdf[[index]], bdf$factors, min),
                       max = tapply(bdf[[index]], bdf$factors, max))

  # Model with the lowest index across folds
  lowmod <- lapply(k.fits, function(x) x[x[[index]] == min(x[[index]]),])
  lowmod.df <- as.data.frame(Reduce(rbind,lowmod))
  lowmod <- as.data.frame(table(lowmod.df$factors))
  names(lowmod) <- c("factors", "num_folds_lowest")

  # joining into single table
  fit <- merge(x = fit, y = lowmod, by = "factors", all.x = TRUE)
  fit$num_folds_lowest <- ifelse(is.na(fit$num_folds_lowest), 0, fit$num_folds_lowest)

  return(fit)
}

#' Unique factor structures
#'
#' Extract unique factor structures across the k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#'
#' @return For each factor quantity, a \code{list} containing \code{lavaan} syntax specifying the factor structure and the folds where the structure was identified

model_structure <- function(kfa){

  k <- length(kfa)
  maxfac <- max(unlist(lapply(kfa, length)))

  kstructures <- vector("list", length = maxfac)
  kstructures[[1]][[1]] <- list(structure = efa_cfa_syntax(lavaan::lavInspect(kfa[[1]][[1]], "est")$lambda),
                                folds = 1:k)
  for(m in 2:maxfac){
    structures <- vector("list", length = k)
    for(f in 1:k){
      structures[[f]] <- efa_cfa_syntax(lavaan::lavInspect(kfa[[f]][[m]], "est")$lambda)
    }

    us <- unique(unlist(structures, use.names = FALSE))

    slist <- vector("list", length = length(us))
    for(u in seq_along(us)){

      folds <- which(unlist(lapply(structures, function(x) x == us[[u]])))

      slist[[u]] <- list(structure = us[[u]],
                         folds = which(unlist(lapply(structures, function(x) x == us[[u]]))))
    }

    kstructures[[m]] <- slist
  }
  return(kstructures)
}
