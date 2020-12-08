
# Not positive yet what will be included here
k_cfa <- function(model, data, ordered = NULL, missing = "listwise", standardized = FALSE){

  ## obtaining test sample for cfa
  cfa.sample <- items[!(1:nrow(items) %in% trainfolds[[fold]]), ]

  ## running cfa
  # add ...
  cfa.mod <- lavaan::cfa(model = model,
                         data = data,
                         ordered = ordered,
                         missing = missing,
                         ...)

  ## model fit indices - could add indices as an optional argument
  indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
               'cfi.scaled', 'tli.scaled', 'srmr',
               'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled')
  fit <- as.data.frame(t(c(lavaan::fitMeasures(cfa.mod, fit.measures = indices))))
  names(fit) <- gsub("\\.scaled", "", names(fit))

  if(!is.na(object@loglik$loglik)){
    fit$AIC <- round(AIC(cfa.mod), 1)
  }

  # Note: se are unstandardized even if standardized = TRUE
  loadings <- lavaan::parameterEstimates(cfa.mod, se = TRUE,
                                         zstat = FALSE, pvalue = FALSE, ci = FALSE,
                                         standardized = standardized)
  loadings <- subset(loadings, op == "=~")

  cfaout <- list(fit = fitind,
                 loadings = loadings)


  return(cfaout)
}

