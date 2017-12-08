#' Extract the birth rate distribution from a Birth-Death tree prior
#' @inheritParams default_params_doc
#' @return the birth rate distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_bd_birth_rate_distr <- function(bd_tree_prior) {

  if (!is_bd_tree_prior(bd_tree_prior)) {
    stop("bd_tree_prior must be a bd_tree_prior")
  }
  testit::assert("birth_rate_distr" %in% names(bd_tree_prior))
  bd_tree_prior$birth_rate_distr
}
