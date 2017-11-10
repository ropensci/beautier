#' Extract the death rate distribution from a death-Death tree prior
#' @param bd_tree_prior a Yule tree prior, as created
#'   by \code{\link{create_bd_tree_prior}}
#' @return the death rate distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_bd_death_rate_distr <- function(bd_tree_prior) {

  if (!is_bd_tree_prior(bd_tree_prior)) {
    stop("bd_tree_prior must be a bd_tree_prior")
  }
  testit::assert("death_rate_distr" %in% names(bd_tree_prior))
  bd_tree_prior$death_rate_distr
}
