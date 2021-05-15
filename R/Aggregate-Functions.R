#' Aggregated factor loadings
#'
#' The factor loadings aggregated over k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#' @param flag threshold below which loading will be flagged
#'
#' @return \code{data.frame} of mean factor loadings for each factor model and \code{vector} with count of folds with a flagged loading

agg_loadings <- function(kfa, flag = .30){

  kfa <- kfa$cfas

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))
  vnames <- dimnames(lavaan::lavInspect(kfa[[1]][[1]], "sampstat")$cov)[[1]]

  klambdas <- vector("list", m)
  kflag <- vector("integer", m)
  for(n in 1:m){
    lambdas <- data.frame()
    load.flag <- vector("integer", k)
    for(f in 1:k){
      loads <- subset(lavaan::standardizedSolution(kfa[[f]][[n]], "std.lv",
                                                   se = FALSE, zstat = FALSE,
                                                   pvalue = FALSE, ci = FALSE),
                      op == "=~")
      lambdas <- rbind(lambdas, loads)

      load.flag[[f]] <- sum(loads$est.std < flag)
    }

    klambdas[[n]] <- data.frame(variable = vnames,
                                mean = tapply(lambdas$est.std, lambdas$rhs, mean),
                                min = tapply(lambdas$est.std, lambdas$rhs, min),
                                max = tapply(lambdas$est.std, lambdas$rhs, max),
                                `folds flagged` = tapply(lambdas$est.std, lambdas$rhs, function(x) sum(x < flag)),
                                check.names = FALSE)

    ## count of folds with a loading under flag threshold
    kflag[[n]] <- sum(load.flag > 0)
  }
  names(klambdas) <- names(kfa[[1]])

  return(list(loadings = klambdas,
              flag = kflag))
}


#' Aggregate parameter and stability
#'
#' Internal function for aggregating a parameter over k-folds following Rubin's (1987) rules and calculating the stability of the parameter.
#'
#' @param est vector of parameter estimates
#' @param var vector of variances for \code{est}
#' @param n number of observations per fold
#' @param alpha type I error rate for one-sided test
#' @param output.var include all variance components in the output
#'
#' @details
#' The stability index is calculated as 1 - bv / tv where bv is the between fold variance and tv is the total pooled variance.
#'
#' @return a single row \code{data.frame}

agg_param <- function(est, var, n, alpha = .05, output.var = FALSE){

  k <- length(est)
  bar <- mean(est)
  wv <- mean(var)
  bv <- var(est)
  tv <- wv + (1 + k)*bv/k
  stability <- 1 - bv / tv

  # degrees of freedom
  rd <- (1 + 1/k) * bv/tv
  df <- (k - 1) / rd^2
  dfadj <- (n + 1) / (n + 3) * n * (1 - rd)
  df <- df * dfadj / (df + dfadj)

  # confidence interval
  tcrit <- qt(alpha, df)
  ci.lo <- bar - sqrt(tv) * tcrit
  ci.hi <- bar + sqrt(tv) * tcrit

  table <- data.frame(parameter = bar, ci.lo = ci.lo, ci.hi = ci.hi, stability = stability)

  if(output.var == TRUE){
  table <- cbind(table, data.frame(var.within = wv, var.between = bv, var.total = tv))
  }

  return(table)
}


#' Aggregated factor correlations
#'
#' The factor correlations aggregated over k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#' @param type currently ignored; \code{"factor"} (default) or \code{"observed"} variable correlations
#' @param flag threshold above which a factor correlation will be flagged
#'
#' @return a \code{data.frame} of mean factor correlations for each factor model and \code{vector} with count of folds with a flagged correlation

agg_cors <- function(kfa, type = "factor", flag = .90){

  kfa <- kfa$cfas

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))

  kcorrs <- vector("list", m)
  kflag <- vector("integer", m)
  # Currently assumes the first element is a 1 factor model; need a more robust check
  kcorrs[[1]] <- NULL
  kflag[[1]] <- NA
  for(n in 2:m){
    cor.lv <- vector("list", k) # latent variable correlation matrix
    cor.flag <- vector("integer", k) # count of correlations above flag threshold
    for(f in 1:k){
      cor.lv[[f]] <- lavaan::lavInspect(kfa[[f]][[n]], "cor.lv")
      cor.flag[[f]] <- sum(cor.lv[[f]] > flag) - nrow(cor.lv[[f]])
    }

    ## mean correlation across folds
    aggcorrs <- Reduce(`+`, cor.lv) / length(cor.lv)
    aggcorrs[upper.tri(aggcorrs, diag = FALSE)] <- NA
    aggcorrs <- cbind(data.frame(rn = row.names(aggcorrs)), aggcorrs)
    kcorrs[[n]] <- aggcorrs

    ## count of folds with a correlation over flag threshold
    kflag[[n]] <- sum(cor.flag > 0)

  }
  names(kcorrs) <- names(kfa[[1]])

 return(list(correlations = kcorrs,
             flag = kflag))
}

#' Aggregated scale reliabilities
#'
#' The factor reliabilities aggregated over k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#' @param flag threshold below which reliability will be flagged
#'
#' @return a \code{data.frame} of mean factor (scale) reliabilities for each factor model and \code{vector} with count of folds with a flagged reliability

agg_rels <- function(kfa, flag = .60){

  kfa <- kfa$cfas

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))

  krels <- vector("list", m)
  kflag <- vector("integer", m)
  for(n in 1:m){
    rels <- vector("list", k)
    rel.flag <- vector("integer", k)
    for(f in 1:k){
      rels[[f]] <- suppressMessages(semTools::reliability(kfa[[f]][[n]])[c(1,4),])
      if(n == 1){
        rel.flag[[f]] <- sum(rels[[f]][[2]] < flag) # flag based on omega, not alpha
      } else{
        rel.flag[[f]] <- sum(rels[[f]][2,] < flag) # flag based on omega, not alpha
      }
    }

    ## mean reliability across folds
    krels[[n]] <- Reduce(`+`, rels) / length(rels)
    if(n == 1){
      krels[[n]] <- as.data.frame(krels[[n]])
      names(krels[[n]]) <- "f1"
    }
    krels[[n]] <- cbind(data.frame(type = c("alpha", "omega_h")), krels[[n]])

    ## count of folds with a reliabilities below flag threshold
    kflag[[n]] <- sum(rel.flag > 0)
  }
  names(krels) <- names(kfa[[1]])

  return(list(reliabilities = krels,
              flag = kflag))
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

  kfa <- kfa$cfas
  k <- length(kfa) # number of folds
  m <- max(unlist(lapply(kfa, length))) # number of models per fold
  model.names <- names(kfa[[1]])
  index <- if(sum(grepl("df", index)) == 0) c("df", index) else index

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
      fits.df <- cbind(data.frame(model = model.names),
                       as.data.frame(Reduce(rbind, fits[[f]])))
      row.names(fits.df) <- NULL
      kfits[[f]] <- fits.df
    }
  } else if(by.fold == FALSE){

    kfits <- vector("list", length = m)
    for(n in 1:m){
      fits.df <- cbind(data.frame(fold = as.character(1:k)),
                       as.data.frame(Reduce(rbind, lapply(fits, "[[", n)))) #function(x) x[[n]]
      row.names(fits.df) <- NULL
      kfits[[n]] <- fits.df
    }
    names(kfits) <- model.names
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
  model.names <- kfits[[1]]$model
  # m <- max(unlist(lapply(kfits, nrow)))

  bdf <- as.data.frame(Reduce(rbind, kfits))

  fit <- data.frame(model = model.names,
                    df = tapply(bdf[["df"]], bdf$model, mean))
  for(i in index){

    agg <- data.frame(factors = model.names,
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

#' Model with best fit
#'
#' Summary table of best fitting model in k-folds
#'
#' @param kfits An object returned from \code{\link[kfa]{k_model_fit}} when \code{by.folds = TRUE}
#' @param index One or more fit indices to summarize in the report. Default are "chisq", "cfi", and "rmsea".
#'
#' @return \code{data.frame} with fold in rows, index in columns, and factor model with best fit in the cells

best_model <- function(kfits, index = c("chisq", "cfi", "rmsea")){

  k <- length(kfits) # number of folds
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
    appendix.df <- merge(appendix.df, appendix[[i]], by = "fold")
  }
  appendix.df$fold <- factor(appendix.df$fold, levels = c(1:k, "Mean"))
  appendix.df <- appendix.df[order(appendix.df$fold),]
  row.names(appendix.df) <- NULL

  return(appendix.df)
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

#' Unique factor structures
#'
#' Extract unique factor structures across the k-folds
#'
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#' @param which Currently only for internal use; Should the unique structures be extracted from the "cfa" \code{lavaan} models or "efa" syntax?
#'
#' @return For each unique structure within each factor model, a \code{list} containing \code{lavaan} syntax specifying the factor structure and the folds where the structure was identified

model_structure <- function(kfa, which = "cfa"){

  if(which == "cfa"){
    kfa <- kfa$cfas
  } else if(which == "efa"){
    kfa <- kfa$efas
  }

  k <- length(kfa)
  m <- max(unlist(lapply(kfa, length)))

  kstructures <- vector("list", length = m)

  # cfa requires extracting factor loadings (lavInspect)
  if(which == "cfa"){

    kstructures[[1]][[1]] <- list(structure = efa_cfa_syntax(lavaan::lavInspect(kfa[[1]][[1]], "std")$lambda),
                                  folds = 1:k)
    for(n in 2:m){
      structures <- vector("list", length = k)
      for(f in 1:k){
        structures[[f]] <- efa_cfa_syntax(lavaan::lavInspect(kfa[[f]][[n]], "std")$lambda)
      }
      kstructures[[n]] <- match_structure(structures)
    }
  } else if(which == "efa"){

    # currently assumes 1-factor structure exists and is the same over folds
    kstructures[[1]][[1]] <- list(structure = kfa[[1]][[1]], folds = 1:k)
    for(n in 2:m){
      structures <- vector("list", length = k)
      for(f in 1:k){
        structures[[f]] <- kfa[[f]][[n]]
      }
      kstructures[[n]] <- match_structure(structures)
    }
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
#' @param kfa An object returned from \code{\link[kfa]{kfa}}
#' @param corrs An object returned from \code{\link[kfa]{agg_cors}}
#' @param rels An object returned from \code{\link[kfa]{agg_rels}}
#'
#' @return a \code{data.frame}

model_flags <- function(kfa, cors, rels, loads){

  # Multiple structures from EFA
  multistrux <- data.frame(model = names(kfa$efa.structures),
                           `multiple structures` = unlist(lapply(kfa$efa.structures, function(x) length(x) > 1)),
                           check.names = FALSE)

  # flags from CFA models
  cfas <- kfa$cfas
  k <- length(cfas)
  m <- max(unlist(lapply(cfas, length)))

  ## Flagging non-convergence and heywood case warnings/errors
  cnvgd <- vector("list", k)
  hey <- vector("list", k)
  for(f in 1:k){

    ## count of heywood cases in each model - use lavInspect(kstudent$cfas[[1]]$Break, "post.check") instead
    hey[[f]] <- lapply(kstudent$cfas[[f]], function(x)
      nrow(subset(lavaan::parameterEstimates(x, standardized = FALSE,
                                             se = FALSE, zstat = FALSE,
                                             pvalue = FALSE, ci = FALSE),
                  op == "~~" & lhs == rhs & est < 0))) # keeps negative residual or factor variance

    ## convergence status of each model
    cnvgd[[f]] <- lapply(kstudent$cfas[[f]], lavaan::lavInspect, "converged")

  }

  nonconvergence <- integer(length = m)
  heywood <- integer(length = m)
  for(n in 1:m){

    nonconvergence[[n]] <- sum(unlist(lapply(cnvgd, '[[', n)) == FALSE)
    heywood[[n]] <- sum(unlist(lapply(hey, '[[', n)) > 0)
  }

  ## joining flags into data.frame
  flags <- data.frame(model = names(cfas[[1]]),
                      `non-convergence` = nonconvergence,
                      `heywood case` = heywood,
                      `high factor correlation` = cors$flag,
                      `low scale reliability` = rels$flag,
                      `low loading` = loads$flag,
                      check.names = FALSE)

  flags <- merge(multistrux, flags, by = "model", all.x = FALSE, all.y = TRUE, sort = FALSE)

  return(flags)
}

