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

kfa_report <- function(kfa, file.name, report.format = "html_document", report.title = file.name,
                       cut = .3){

  ## analysis summary info
  k <- length(kfa)
  nobs <- sum(unlist(lapply(kfa, function(x) lavaan::lavInspect(x[[1]], "nobs"))))
  vnames <- dimnames(lavaan::lavInspect(kfa[[1]][[1]], "sampstat")$cov)[[1]]
  nvars <- length(vnames)
  maxfac <- max(unlist(lapply(kfa, length)))

  ## model fit - dataframe for each fold
  kfits <- k_model_fit(kfa)

  ## summarizing fit statistics by factor (currently based on RMSEA)
  fit.table <- agg_model_fit(kfits, index = "rmsea")

  ## model structures
  kstructures <- model_structure(kfa)

  ## loadings
  klambdas <- agg_loadings(kfa)

  ## factor correlations
  kcorrs <- agg_f_cor(kfa)


  ## running report
  template <- system.file("rmd", "kfa-report.Rmd", package = "kfa")
  dir <- getwd()
  rmarkdown::render(input = template,
                    output_format = report.format,
                    output_file = file.name,
                    output_dir = dir,
                    output_options = list(toc = TRUE))
}


flextab_format <- function(df, bold.type = "none", cut = .3, digits = 2){

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
