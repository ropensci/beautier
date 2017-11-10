#' Get the distribution names
#' @return the distribution names
#' @examples
#'   names <- get_distr_names()
#'   testthat::expect_true("uniform" %in% names)
#'   testthat::expect_true("normal" %in% names)
#'   testthat::expect_true("one_div_x" %in% names)
#'   testthat::expect_true("log_normal" %in% names)
#'   testthat::expect_true("exponential" %in% names)
#'   testthat::expect_true("gamma" %in% names)
#'   testthat::expect_true("beta" %in% names)
#'   testthat::expect_true("laplace" %in% names)
#'   testthat::expect_true("inv_gamma" %in% names)
#'   testthat::expect_true("poisson" %in% names)
#' @author Richel J.C. Bilderbeek
#' @export
get_distr_names <- function() {
  return(c("uniform", "normal", "one_div_x", "log_normal", "exponential",
    "gamma", "beta", "laplace", "inv_gamma", "poisson"))
}
