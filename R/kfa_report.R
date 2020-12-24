#' Creates summary report from a k-fold factor analysis
#'
#' Generates a report summarizing the factor analytic results over k-folds.
#'
#' @param kfa an object returned from \code{kfa} or otherwise defined list of
#' lists where the inner list consists of \code{lavaan} objects.
#' @param report.title title of the report
#' @param file.name character; file name to create on disk.
#' @param cut numeric; standardized factor loadings below this value are emphasized in
#' tables and figures.
#'
#' @return html report generated via rmarkdown

kfa_report <- function(kfa, report.title = "K-Fold Factor Analysis",
                       file.name = NULL, cut = .3){

  #### Analysis Summary Info ####
  k <- length(kfa)
  nobs <- sum(unlist(lapply(kfa, function(x) lavaan::lavInspect(x[[1]], "nobs"))))
  vnames <- dimnames(lavaan::lavInspect(kfa[[1]][[1]], "sampstat")$cov)[[1]]
  nvars <- length(vnames)
  maxfac <- max(unlist(lapply(kfa, length)))

  #### Fit information  ####
  ## Gathering fit - dataframe for each fold
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

  ## summarizing fit statistics by factor (currently based on RMSEA)
  bdf <- as.data.frame(Reduce(rbind, kfits))
  aggfit <- data.frame(factors = 1:maxfac,
                       mean = tapply(bdf$rmsea, bdf$factors, mean),
                       min = tapply(bdf$rmsea, bdf$factors, min),
                       max = tapply(bdf$rmsea, bdf$factors, max))

  # Model with the lowest RMSEA across folds
  lowmod <- lapply(kfits, function(x) x[x$rmsea == min(x$rmsea),])
  lowmod.df <- as.data.frame(Reduce(rbind,lowmod))
  lowmod <- as.data.frame(table(lowmod.df$factors))
  names(lowmod) <- c("factors", "low_in_fold")

  # joining into single table
  aggfit <- merge(x = aggfit, y = lowmod, by = "factors", all.x = TRUE)
  aggfit$low_in_fold <- ifelse(is.na(aggfit$low_in_fold), 0, aggfit$low_in_fold)


  #### Structures ####

  ##
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

  ## loadings
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

  ## factor correlations
  kcorrs <- vector("list", maxfac)
  kcorrs[[1]] <- NULL
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


  #### Naming output file and running report ####
  template <- system.file("rmd", "kfa-report.Rmd", package = "kfa")
  dir <- getwd()

  if(is.null(file.name)){
    file.name <- "K-Fold Factor Analysis Report.html"
  } else if(!grepl("\\.html", file.name)){
    file.name <- paste0(file.name, ".html")
  }
  rmarkdown::render(input = template,
                    output_file = file.name,
                    output_dir = dir)
}


flextab_format <- function(df, bold.type = "none", cut = .3, digits = 2){

  numericcols <- which(unlist(lapply(df, is.numeric)))

  ftab <- flextable(df)
  ftab <- colformat_num(ftab, j = numericcols, digits = digits)
  ftab <- align(ftab, i = NULL, j = NULL, align = "center", part = "all")

  if(bold.type == "rmsea"){
    ftab <- bold(ftab, i = ~rmsea == min(rmsea), part =  "body")
  } else if(bold.type == "lambda"){
    ftab <- bold(ftab, i = ~mean < .3, part = "body")
    # error occurs when .3 is replaced by cut; not sure why
  }

  ftab <- autofit(ftab)

  return(ftab)
}
