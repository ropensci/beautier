#' Get the tree prior names
#' @return the tree prior names
#' @seealso Use \link{create_tree_priors} to get all tree priors
#' @examples
#'   names <- beautier:::get_tree_prior_names()
#'   testit::assert("birth_death" %in% names)
#'   testit::assert("coalescent_bayesian_skyline" %in% names)
#'   testit::assert("coalescent_constant_population" %in% names)
#'   testit::assert("coalescent_exp_population" %in% names)
#'   testit::assert("yule" %in% names)
#' @author Richèl J.C. Bilderbeek
#' @export
get_tree_prior_names <- function() {
  c(
    "birth_death",
    "coalescent_bayesian_skyline",
    "coalescent_constant_population",
    "coalescent_exp_population",
    "yule"
  )
}
