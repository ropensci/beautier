#' Extract the Coalescent Exponential Population tree prior's
#' population size distribution
#' @param cep_tree_prior a cep tree prior, as created
#'   by \code{\link{create_cep_tree_prior}}
#' @return the birth rate distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_cep_pop_size_distr <- function(cep_tree_prior) {

  if (!is_cep_tree_prior(cep_tree_prior)) {
    stop("cep_tree_prior must be a cep_tree_prior")
  }
  testit::assert("pop_size_distr" %in% names(cep_tree_prior))
  cep_tree_prior$pop_size_distr
}
