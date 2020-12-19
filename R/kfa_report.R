#' Creates summary report from a k-fold factor analysis
#'
#' Generates a report summarizing the factor analytic results over k-folds.
#'
#' @param kfa an object returned from \code{kfa} or otherwise defined list of
#' lists where the inner list consists of \code{lavaan} objects.
#' @param instrument.name character; name given to the set of variables used in the
#' factor analysis and printed in the title of the report.
#' @param file.name character;  file name to create on disk. The default is
#' "kfa - \code{instrument.name}".
#'
#' @return html report generated via rmarkdown

kfa_report <- function(kfa, report.title = NULL, file.name = NULL){

  ## Analysis Summary Info
  k <- length(kfa)
  vnames <- dimnames(lavaan::lavInspect(kfa[[1]][[1]], "sampstat")$cov)
  nvars <- length(vnames[[1]])
  maxfac <- max(unlist(lapply(kfa, length)))

  ## Fit information
  kfits <- vector("list", length = k)
  for(f in 1:k){

    fits <- lapply(kfa[[f]], function(x) {
      lavaan::fitmeasures(x, c("chisq", "df", "cfi",
                               "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
      })

    fits.df <- cbind(data.frame(model = as.character(1:length(fits))),
                   as.data.frame(Reduce(rbind, fits)))
    row.names(fits.df) <- NULL

    kfits[[f]] <- fits.df
  }

  ## Model with the lowest RMSEA across folds
  lowmod <- lapply(kfits, function(x) x[x$rmsea == min(x$rmsea),])
  lowmod.df <- as.data.frame(Reduce(rbind,lowmod))
  lowmod <- as.data.frame(table(lowmod.df$model))
  names(lowmod) <- c("model", "folds")

  ## Naming output file and running report

  if(is.null(report.title)){
    report.title <- "K-Fold Factor Analysis"
  }
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


flextab_format <- function(df, bold = TRUE, digits = 2){

  numericcols <- which(unlist(lapply(df, is.numeric)))

  ftab <- flextable(df)
  ftab <- colformat_num(ftab, j = numericcols, digits = digits)
  ftab <- align(ftab, i = NULL, j = NULL, align = "center", part = "all")

  if(bold == TRUE){
    ftab <- bold(ftab, i = ~rmsea == min(rmsea), part =  "body")
  }

  ftab <- autofit(ftab)

  return(ftab)
}
