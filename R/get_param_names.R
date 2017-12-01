#' Get the parameter names
#' @return the parameter names
#' @author Richel J.C. Bilderbeek
#' @examples
#'   names <- get_param_names()
#'   testthat::expect_true("alpha" %in% names)
#'   testthat::expect_true("beta" %in% names)
#'   testthat::expect_true("clock_rate" %in% names)
#'   testthat::expect_true("kappa_1" %in% names)
#'   testthat::expect_true("kappa_2" %in% names)
#'   testthat::expect_true("lambda" %in% names)
#'   testthat::expect_true("m" %in% names)
#'   testthat::expect_true("mean" %in% names)
#'   testthat::expect_true("mu" %in% names)
#'   testthat::expect_true("rate_ac" %in% names)
#'   testthat::expect_true("rate_ag" %in% names)
#'   testthat::expect_true("rate_at" %in% names)
#'   testthat::expect_true("rate_cg" %in% names)
#'   testthat::expect_true("rate_ct" %in% names)
#'   testthat::expect_true("rate_gt" %in% names)
#'   testthat::expect_true("s" %in% names)
#'   testthat::expect_true("scale" %in% names)
#'   testthat::expect_true("sigma" %in% names)
#' @export
get_param_names <- function() {
  return(
    c(
      "alpha",
      "beta",
      "clock_rate",
      "lambda",
      "kappa_1",
      "kappa_2",
      "m",
      "mean",
      "mu",
      "rate_ac",
      "rate_ag",
      "rate_at",
      "rate_cg",
      "rate_ct",
      "rate_gt",
      "s",
      "scale",
      "sigma"
    )
  )
}
