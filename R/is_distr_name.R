#' Determines if the name is a valid distribution name
#' @param name the name to be tested
#' @return TRUE if the name is a valid distribution name, FALSE otherwise
#' @examples
#' library(testthat)
#'
#' expect_true(is_distr_name("uniform"))
#' expect_true(is_distr_name("normal"))
#' expect_true(is_distr_name("one_div_x"))
#' expect_true(is_distr_name("log_normal"))
#' expect_true(is_distr_name("exponential"))
#' expect_true(is_distr_name("gamma"))
#' expect_true(is_distr_name("beta"))
#' expect_true(is_distr_name("laplace"))
#' expect_true(is_distr_name("inv_gamma"))
#' expect_true(is_distr_name("poisson"))
#' @author Richèl J.C. Bilderbeek
#' @export
is_distr_name <- function(name) {
  name %in% beautier::get_distr_names()
}
