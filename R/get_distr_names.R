#' Get the distribution names
#' @return the distribution names
#' @examples
#' library(testthat)
#'
#' names <- get_distr_names()
#'
#' expect_true("uniform" %in% names)
#' expect_true("normal" %in% names)
#' expect_true("one_div_x" %in% names)
#' expect_true("log_normal" %in% names)
#' expect_true("exponential" %in% names)
#' expect_true("gamma" %in% names)
#' expect_true("beta" %in% names)
#' expect_true("laplace" %in% names)
#' expect_true("inv_gamma" %in% names)
#' expect_true("poisson" %in% names)
#' @author Richèl J.C. Bilderbeek
#' @export
get_distr_names <- function() {
  c(
    "uniform", "normal", "one_div_x", "log_normal", "exponential",
    "gamma", "beta", "laplace", "inv_gamma", "poisson"
  )
}
