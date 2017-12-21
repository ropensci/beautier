#' Get the tree prior names
#' @return the tree prior names
#' @examples
#'   names <- beautier:::get_tree_prior_names()
#'   testit::assert("birth_death" %in% names)
#'   testit::assert("coalescent_bayesian_skyline" %in% names)
#'   testit::assert("coalescent_constant_population" %in% names)
#'   testit::assert("coalescent_exp_population" %in% names)
#'   testit::assert("yule" %in% names)
#' @author Richel J.C. Bilderbeek
get_tree_prior_names <- function() {
  c(
    "birth_death",
    "coalescent_bayesian_skyline",
    "coalescent_constant_population",
    "coalescent_exp_population",
    "yule"
  )
}
