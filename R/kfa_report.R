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
#' @param load.flag Item loadings below this value will be flagged. Default is .30
#' @param cor.flag Factor correlations above this value will be flagged. Default is .90
#' @param rel.flag Factor (scale) reliability below this value will be flagged. Default is .60.
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return a summary report of factor structure and model fit within and between folds
#'
#' @export

kfa_report <- function(kfa, file.name, report.format = "html_document", report.title = file.name,
                       word.template = NULL, index = c("chisq", "cfi", "rmsea"),
                       load.flag = .30, cor.flag = .90, rel.flag = .60,
                       digits = 2){

  ## analysis summary info
  k <- length(kfa$cfas) # number of folds
  model.names <- names(kfa$cfas[[1]]) # names(kfa$cfa.syntax)
  fac.allow <- length(kfa$efa.structures)
  fac.max <- max(as.numeric(substring(model.names[grepl("-factor", model.names)], 1, 1)))  # kfa naming convention "#-factor"; custom functions are assumed to have different convention
  m <- max(unlist(lapply(kfa$cfas, length))) # number of models per fold (includes both efa and custom structures); m == length(model.names)
  nobs <- sum(unlist(lapply(kfa$cfas, function(x) lavaan::lavInspect(x[[1]], "nobs"))))
  vnames <- dimnames(lavaan::lavInspect(kfa$cfas[[1]][[1]], "sampstat")$cov)[[1]]
  nvars <- length(vnames)

  #### Model Fit ####
  ## summarizing fit statistics by fold
  kfits <- k_model_fit(kfa, index = index, by.fold = TRUE) # dataframe for each fold
  fit.table <- agg_model_fit(kfits, index = index, digits = 2)

  ## best model in each fold
  # best.model <- best_model(kfits, index = index)

  ## creating appendix -  folds x model table of fit statistics
  mfits <- k_model_fit(kfa, index = index, by.fold = FALSE)
  appendix <- get_appendix(mfits, index = index)

  #### Parameters ####
  ## model structures
  kstructures <- model_structure(kfa, which = "cfa")

  ## loadings
  klambdas <- agg_loadings(kfa, flag = load.flag, digits = digits)

  ## factor correlations
  kcorrs <- agg_cors(kfa, flag = cor.flag)

  ## score reliabilities
  krels <- agg_rels(kfa, flag = rel.flag, digits = digits)

  ## flagged problems
  flagged <- model_flags(kfa, kcorrs, krels, klambdas)

  ## running report
  if(report.format == "word_document"){
    if(is.null(word.template)){
      word.template <- "kfa_word_template.docx"
    }
    width <- 6.5
  } else {
    width <- NULL
  }
  template <- system.file("rmd", "kfa-report.Rmd", package = "kfa")
  dir <- getwd()
  rmarkdown::render(input = template,
                    output_format = report.format,
                    output_file = file.name,
                    output_dir = dir,
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

appendix_wrapper <- function(appendix, mapping, border, digits){

  appendix.flex <- flextable::flextable(appendix)
  appendix.flex <- flextable::colformat_double(appendix.flex, j = -c(1), digits = digits)
  appendix.flex <- two_level_flex(appendix.flex, mapping = mapping, vert.cols = c("fold"), border = border)
  return(appendix.flex)

}
