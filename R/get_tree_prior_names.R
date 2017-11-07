#' Get the tree prior names
#' @return the tree prior names
#' @author Richel J.C. Bilderbeek
#' @examples
#'   names <- get_tree_prior_names()
#'   testthat::expect_true("birth_death" %in% names)
#'   testthat::expect_true("coalescent_bayesian_skyline" %in% names)
#'   testthat::expect_true("coalescent_constant_population" %in% names)
#'   testthat::expect_true("coalescent_exponential_population" %in% names)
#'   testthat::expect_true("yule" %in% names)
#' @export
get_tree_prior_names <- function() {
  return(
    c(
      "birth_death",
      "coalescent_bayesian_skyline",
      "coalescent_constant_population",
      "coalescent_exponential_population",
      "yule"
    )
  )
}
