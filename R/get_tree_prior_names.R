#' Get the tree prior names
#' @return the tree prior names
#' @seealso Use \link{create_tree_priors} to get all tree priors
#' @examples
#' library(testthat)
#'
#' names <- beautier:::get_tree_prior_names()
#' expect_true("birth_death" %in% names)
#' expect_true("coalescent_bayesian_skyline" %in% names)
#' expect_true("coalescent_constant_population" %in% names)
#' expect_true("coalescent_exp_population" %in% names)
#' expect_true("yule" %in% names)
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
