% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Aggregate-Functions.R
\name{agg_rels}
\alias{agg_rels}
\title{Aggregated scale reliabilities}
\usage{
agg_rels(models, flag = 0.6, digits = 2)
}
\arguments{
\item{models}{An object returned from \code{\link[kfa]{kfa}}}

\item{flag}{threshold below which reliability will be flagged}

\item{digits}{integer; number of decimal places to display in the report.}
}
\value{
\code{data.frame} of mean factor (scale) reliabilities for each factor model and \code{vector} with count of folds with a flagged reliability
}
\description{
The factor reliabilities aggregated over k-folds
}
\examples{
data(example.kfa)
agg_rels(example.kfa)

}
