#' Extract the Coalescent Contant Population tree prior's
#' population size distribution
#' @param ccp_tree_prior a ccp tree prior, as created
#'   by \code{\link{create_ccp_tree_prior}}
#' @return the birth rate distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_ccp_pop_size_distr <- function(ccp_tree_prior) {

  if (!is_ccp_tree_prior(ccp_tree_prior)) {
    stop("ccp_tree_prior must be a ccp_tree_prior")
  }
  testit::assert("pop_size_distr" %in% names(ccp_tree_prior))
  ccp_tree_prior$pop_size_distr
}
