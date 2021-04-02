#' Creates summary report from a k-fold factor analysis
#'
#' Generates a report summarizing the factor analytic results over k-folds.
#'
#' @param kfa An object returned from \code{kfa} or otherwise defined list of
#' lists where the inner list consists of \code{lavaan} objects.
#' @param file.name Character; file name to create on disk.
#' @param report.format File format of the report. Default is HTML ("html_document"). See \code{\link[rmarkdown]{render}} for other options.
#' @param report.title Title of the report
#' @param index One or more fit indices to summarize in the report. The degrees of freedom are always reported. Default are "chisq", "cfi", and "rmsea".
#' @param cut numeric; standardized factor loadings below this value are emphasized in tables and figures.
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return a summary report of factor structure and model fit within and between folds

kfa_report <- function(kfa, file.name, report.format = "html_document", report.title = file.name,
                       index = c("chisq", "cfi", "rmsea"), cut = .3, digits = 2){

  ## analysis summary info
  k <- length(kfa) # number of folds
  m <- max(unlist(lapply(kfa, length))) # largest factor model
  nobs <- sum(unlist(lapply(kfa, function(x) lavaan::lavInspect(x[[1]], "nobs"))))
  vnames <- dimnames(lavaan::lavInspect(kfa[[1]][[1]], "sampstat")$cov)[[1]]
  nvars <- length(vnames)


  #### Model Fit ####
  ## summarizing fit statistics by fold
  kfits <- k_model_fit(kfa, index = index, by.fold = TRUE) # dataframe for each fold
  fit.table <- agg_model_fit(kfits, index = index)

  ## best model in each fold
  best.model <- best_model(kfits, index = index)

  ## creating appendix -  folds x model table of fit statistics
  mfits <- k_model_fit(kfa, index = index, by.fold = FALSE)
  appendix <- get_appendix(mfits, index = index)

  #### Parameters ####
  ## model structures
  kstructures <- model_structure(kfa)

  ## loadings
  klambdas <- agg_loadings(kfa)

  ## factor correlations
  kcorrs <- agg_fac_cor(kfa)

  ## score reliabilities
  krels <- agg_reliability(kfa)

  ## running report
  template <- system.file("rmd", "kfa-report.Rmd", package = "kfa")
  dir <- getwd()
  rmarkdown::render(input = template,
                    output_format = report.format,
                    output_file = file.name,
                    output_dir = dir,
                    output_options = list(toc = TRUE))
}


flextab_format <- function(df, bold.type = "none", cut = .3, digits = digits){

  numericcols <- which(unlist(lapply(df, is.numeric)))

  ftab <- flextable(df)
  ftab <- colformat_double(ftab, j = numericcols, digits = digits)
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
