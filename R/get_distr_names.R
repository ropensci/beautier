#' Get the distribution names
#' @return the distribution names
#' @examples
#'   names <- beautier:::get_distr_names()
#'   testit::assert("uniform" %in% names)
#'   testit::assert("normal" %in% names)
#'   testit::assert("one_div_x" %in% names)
#'   testit::assert("log_normal" %in% names)
#'   testit::assert("exponential" %in% names)
#'   testit::assert("gamma" %in% names)
#'   testit::assert("beta" %in% names)
#'   testit::assert("laplace" %in% names)
#'   testit::assert("inv_gamma" %in% names)
#'   testit::assert("poisson" %in% names)
#' @author Richèl J.C. Bilderbeek
#' @noRd
get_distr_names <- function() {
  c(
    "uniform", "normal", "one_div_x", "log_normal", "exponential",
    "gamma", "beta", "laplace", "inv_gamma", "poisson"
  )
}
