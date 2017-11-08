#' Extract the Coalescent Exponential Population tree prior's
#' growth rate distribution
#' @param cep_tree_prior a cep tree prior, as created
#'   by \code{\link{create_cep_tree_prior}}
#' @return the growth rate distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_cep_growth_rate_distr <- function(cep_tree_prior) {

  if (!is_cep_tree_prior(cep_tree_prior)) {
    stop("cep_tree_prior must be a cep_tree_prior")
  }
  testit::assert("growth_rate_distribution" %in% names(cep_tree_prior))
  cep_tree_prior$growth_rate_distribution
}
