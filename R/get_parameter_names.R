#' Get the parameter names
#' @return the parameter names
#' @author Richel J.C. Bilderbeek
#' @examples
#'   names <- get_parameter_names()
#'   testthat::expect_true("alpha" %in% names)
#'   testthat::expect_true("beta" %in% names)
#'   testthat::expect_true("clock_rate" %in% names)
#'   testthat::expect_true("lambda" %in% names)
#'   testthat::expect_true("m" %in% names)
#'   testthat::expect_true("mean" %in% names)
#'   testthat::expect_true("mu" %in% names)
#'   testthat::expect_true("s" %in% names)
#'   testthat::expect_true("scale" %in% names)
#'   testthat::expect_true("sigma" %in% names)
#' @export
get_parameter_names <- function() {
  return(
    c(
      "alpha",
      "beta",
      "clock_rate",
      "lambda",
      "m",
      "mean",
      "mu",
      "s",
      "scale",
      "sigma"
    )
  )
}
