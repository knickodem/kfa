#' Creates summary report from a k-fold factor analysis
#'
#' Generates a report summarizing the factor analytic results over k-folds.
#'
#' @param models an object returned from \code{\link[kfa]{kfa}}
#' @param file.name character; file name to create on disk.
#' @param report.title character; title of the report
#' @param path character; path of the directory where summary report will be saved. Default is working directory. \code{path} and \code{file.name} are combined to create final file path
#' @param report.format character; file format of the report. Default is HTML ("html_document"). See \code{\link[rmarkdown]{render}} for other options.
#' @param word.template character; file path to word document to use as a formatting template when \code{report.format = "word_document"}.
#' @param index character; one or more fit indices to summarize in the report. Use \code{\link[kfa]{index_available}} to see choices.
#' Chi-square value and degrees of freedom are always reported. Default is CFI and RMSEA (naive, scaled, or robust version depends on estimator used in \code{models}).
#' @param load.flag numeric; factor loadings of variables below this value will be flagged. Default is .30
#' @param cor.flag numeric; factor correlations above this value will be flagged. Default is .90
#' @param rel.flag numeric; factor (scale) reliabilities below this value will be flagged. Default is .60.
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return a summary report of factor structures and model fit within and between folds
#'
#' @examples
#'
#' # simulate data based on a 3-factor model with standardized loadings
#' sim.mod <- "f1 =~ .7*x1 + .8*x2 + .3*x3 + .7*x4 + .6*x5 + .8*x6 + .4*x7
#'                 f2 =~ .8*x8 + .7*x9 + .6*x10 + .5*x11 + .5*x12 + .7*x13 + .6*x14
#'                 f3 =~ .6*x15 + .5*x16 + .9*x17 + .4*x18 + .7*x19 + .5*x20
#'                 f1 ~~ .2*f2
#'                 f2 ~~ .2*f3
#'                 f1 ~~ .2*f3
#'                 x9 ~~ .2*x10"
#' set.seed(1161)
#' sim.data <- simstandard::sim_standardized(sim.mod, n = 900,
#'                                           latent = FALSE,
#'                                           errors = FALSE)[c(2:9,1,10:20)]
#'
#' # include a custom 2-factor model
#' custom2f <- paste0("f1 =~ ", paste(colnames(sim.data)[1:10], collapse = " + "),
#'                    "\nf2 =~ ",paste(colnames(sim.data)[11:20], collapse = " + "))
#'
#' \donttest{
#' mods <- kfa(variables = sim.data,
#'             k = NULL, # prompts power analysis to determine number of folds
#'             cores = 2,
#'             custom.cfas = custom2f)
#'             }
#'
#' \dontrun{
#' kfa_report(mods, file.name = "example_sim_kfa_report",
#'            report.format = "html_document",
#'            report.title = "K-fold Factor Analysis - Example Sim")
#'            }
#'
#' @import lavaan
#' @import rmarkdown
#' @import flextable
#' @importFrom knitr opts_chunk
#' @importFrom knitr knit
#' @importFrom knitr knit_print
#' @importFrom officer fp_border
#' @importFrom semPlot semPaths
#' @importFrom simstandard sim_standardized
#'
#' @export

kfa_report <- function(models,
                       file.name, report.title = file.name,
                       path = NULL,
                       report.format = "html_document",
                       word.template = NULL,
                       index = "default",
                       load.flag = .30, cor.flag = .90, rel.flag = .60,
                       digits = 2){

  if(class(models) == "kfa"){
    cfas <- models$cfas
  } else {
    stop("models must be of class 'kfa'.")
  }

  ## analysis summary info
  k <- length(cfas) # number of folds
  m <- max(unlist(lapply(cfas, length))) # number of models per fold (includes both efa AND custom structures); m == length(mnames)
  mnames <- models$model.names # model names
  fac.allow <- length(models$efa.structures)
  fac.max <- max(as.numeric(substring(mnames[grepl("-factor", mnames)], 1, 1)))  # kfa naming convention "#-factor"; custom functions are assumed to have different convention
  vnames <- dimnames(lavaan::lavInspect(cfas[[1]][[1]], "sampstat")$cov)[[1]] # variable names
  nvars <- length(vnames)
  nobs <- sum(unlist(lapply(cfas, function(x) lavaan::lavInspect(x[[1]], "nobs"))))
  opts <- lavaan::lavInspect(cfas[[1]][[1]], "options") # estimation options; assumed to be the same for all models

  #### Model Fit ####
  ## summarizing fit statistics by fold
  kfits <- k_model_fit(models, index = index, by.fold = TRUE) # dataframe for each fold
  fit.table <- agg_model_fit(kfits, index = "all", digits = 2)

  ## best model in each fold
  # best.model <- best_model(kfits, index = index)

  ## creating appendix -  folds x model table of fit statistics
  mfits <- k_model_fit(models, index = index, by.fold = FALSE)
  appendix <- get_appendix(mfits, index = "all")

  #### Parameters ####
  ## model structures
  kstructures <- model_structure(models)

  ## loadings
  klambdas <- agg_loadings(models, flag = load.flag, digits = digits)

  ## factor correlations
  kcorrs <- agg_cors(models, flag = cor.flag)

  ## score reliabilities
  krels <- agg_rels(models, flag = rel.flag, digits = digits)

  ## flagged problems
  flagged <- model_flags(models, kstructures, klambdas, kcorrs, krels)

  ## running report
  if(report.format == "word_document"){
    width <- 6.5
  } else {
    width <- NULL
  }

  if(is.null(word.template)){
    word.template <- system.file("rmd", "kfa_word_template.docx", package = "kfa")
  }

  if(is.null(path)){
    path <- getwd()
  }
  report.title <- report.title # Unless already specified by user, sets report.title = file.name b/f path gets added
  file.name <- file.path(path, file.name)
  template <- system.file("rmd", "kfa-report.Rmd", package = "kfa")
  rmarkdown::render(input = template,
                    output_format = report.format,
                    output_file = file.name,
                    output_options = list(toc = TRUE, toc_depth = 2,
                                          always_allow_html = TRUE,
                                          reference_docx = word.template))
}


#' Default flextable format
#'
#' Internal function for formatting flextables
#'
#' @param df a \code{data.frame}
#' @param bold.type character indicating table with a pre-specified bolding pattern. Not currently implemented.
#' @param width numeric; maximum width of table in inches.
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return a \code{flextable} object
#'
#' @import flextable
#'
#' @noRd

flextab_format <- function(df, bold.type = "none", width = NULL, digits = 2){

  numericcols <- which(unlist(lapply(df, is.numeric)))

  flex <- flextable::flextable(df)
  flex <- flextable::colformat_double(flex, j = numericcols, digits = digits)
  flex <- flextable::align(flex, i = NULL, j = NULL, align = "center", part = "all")
  flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
  flex <- flextable::padding(flex, padding = 3, part = "all")

  if(bold.type == "fit"){
    flex <- bold(flex, i = ~rmsea == min(rmsea), part =  "body")
  } else if(bold.type == "lambda"){
    flex <- bold(flex, i = ~mean < .3, part = "body")
    # error occurs when .3 is replaced by cut; not sure why
  }

  if(!is.null(width)){
    flex <- flextable::fit_to_width(flex, max_width = width)
  }
  flex <- flextable::autofit(flex)

  return(flex)
}

#' Default flextable format for header with two levels
#'
#' Internal function for formatting flextables
#'
#' @param flex a \code{flextable} object
#' @param mapping a \code{data.frame} specifying header columns. See \code{\link[flextable]{set_header_footer_df}} for details.
#' @param vert.cols columns where level 1 and level 2 cells will be vertically merged. See \code{\link[flextable]{merge_v}} for details.
#' @param border format of of horizontal borders. See \code{\link[flextable]{border_inner_h}} for details.
#'
#' @return a \code{flextable} object
#'
#' @import flextable
#'
#' @noRd

two_level_flex <- function(flex, mapping, vert.cols, border){

  flex <- flextable::set_header_df(flex, mapping = mapping)
  flex <- flextable::merge_h(flex, part = "header")
  flex <- flextable::merge_v(flex, j = vert.cols, part = "header")
  flex <- flextable::fix_border_issues(flex)
  flex <- flextable::border_inner_h(flex, border = border, part = "header")
  flex <- flextable::hline_top(flex, border = border, part = "all")
  # flex <- flextable::theme_vanilla(flex)
  flex <- flextable::align(flex, align = "center", part = "all")
  flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
  flex <- flextable::padding(flex, padding = 3, part = "all")
  flex <- flextable::autofit(flex)
  return(flex)
}


#' Create appendix table
#'
#' Internal function for converting appendix from \code{data.frame} to \code{flextable}
#'
#' @param appendix object returned from \code{\link[kfa]{get_appendix}}
#' @param mapping a \code{data.frame} specifying header columns. See \code{\link[flextable]{set_header_footer_df}} for details.
#' @param border format of of horizontal borders. See \code{\link[flextable]{border_inner_h}} for details.
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return a \code{flextable} object
#'
#' @import flextable
#'
#' @noRd

appendix_wrapper <- function(appendix, mapping, border, digits){

  appendix.flex <- flextable::flextable(appendix)
  appendix.flex <- flextable::colformat_double(appendix.flex, j = -c(1), digits = digits)
  appendix.flex <- two_level_flex(appendix.flex, mapping = mapping, vert.cols = c("fold"), border = border)
  return(appendix.flex)

}
