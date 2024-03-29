% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/k_efa.R
\name{get_std_loadings}
\alias{get_std_loadings}
\title{Standardized factor loadings matrix}
\usage{
get_std_loadings(object, type = "std.all", df = FALSE)
}
\arguments{
\item{object}{a \code{lavaan} object}

\item{type}{standardize on the latent variables (\code{"std.lv"}),
latent and observed variables (\code{"std.all"}, default), or latent and observed variables
but not exogenous variables (\code{"std.nox"})? See \code{\link[lavaan]{standardizedSolution}}.}

\item{df}{should loadings be returned as a \code{matrix} (default) or \code{data.frame}?}
}
\value{
A \code{matrix} or \code{data.frame} of factor loadings
}
\description{
Extract standardized factor loadings from lavaan object
}
\examples{
data(HolzingerSwineford1939, package = "lavaan")
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- lavaan::cfa(HS.model, data = HolzingerSwineford1939)
get_std_loadings(fit)

}
