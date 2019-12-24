#' Get the parameter names
#' @return the parameter names
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' names <- get_param_names()
#' expect_true("alpha" %in% names)
#' expect_true("beta" %in% names)
#' expect_true("clock_rate" %in% names)
#' expect_true("kappa_1" %in% names)
#' expect_true("kappa_2" %in% names)
#' expect_true("lambda" %in% names)
#' expect_true("m" %in% names)
#' expect_true("mean" %in% names)
#' expect_true("mu" %in% names)
#' expect_true("rate_ac" %in% names)
#' expect_true("rate_ag" %in% names)
#' expect_true("rate_at" %in% names)
#' expect_true("rate_cg" %in% names)
#' expect_true("rate_ct" %in% names)
#' expect_true("rate_gt" %in% names)
#' expect_true("s" %in% names)
#' expect_true("scale" %in% names)
#' expect_true("sigma" %in% names)
#' @export
get_param_names <- function() {
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
}
