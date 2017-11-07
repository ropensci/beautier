#' Extract the Yule birth rate distribution a Yule tree prior
#' @param yule_tree_prior a Yule tree prior, as created
#'   by \code{\link{create_yule_tree_prior}}
#' @return the birth rate distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_yule_birth_rate_distr <- function(yule_tree_prior) {

  if (!is_yule_tree_prior(yule_tree_prior)) {
    stop("yule_tree_prior must be a yule_tree_prior")
  }
  testit::assert("birth_rate_distribution" %in% names(yule_tree_prior))
  yule_tree_prior$birth_rate_distribution
}
