#' kfa results from simulated data example
#'
#' Simulated responses for 900 observations on 20 variables loading onto a 3 factor
#' structure (see example in \code{\link[kfa]{kfa}} documentation for model).
#' The simulated data was run through \code{\link[kfa]{kfa}} with the call
#' kfa(sim.data, k = 2, m = 3) which tested 1-, 2-, and 3-factor structures over 2 folds.
#'
#' @usage data(example.kfa)
#'
#' @format An object of class \code{"kfa"}, which is a four-element \code{list}:
#' \itemize{
#' \item **cfas** \code{lavaan} CFA objects for each *k* fold
#' \item **cfa.syntax** syntax used to produce CFA objects
#' \item **model.names** vector of names for CFA objects
#' \item **efa.structures** all factor structures identified in the EFA
#' }
#'
#' @examples
#' data(example.kfa)
#' agg_cors(example.kfa)
"example.kfa"
