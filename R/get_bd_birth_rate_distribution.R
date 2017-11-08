#' Extract the birth rate distribution from a Birth-Death tree prior
#' @param bd_tree_prior a Yule tree prior, as created
#'   by \code{\link{create_bd_tree_prior}}
#' @return the birth rate distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_bd_birth_rate_distr <- function(bd_tree_prior) {

  if (!is_bd_tree_prior(bd_tree_prior)) {
    stop("bd_tree_prior must be a bd_tree_prior")
  }
  testit::assert("birth_rate_distribution" %in% names(bd_tree_prior))
  bd_tree_prior$birth_rate_distribution
}
