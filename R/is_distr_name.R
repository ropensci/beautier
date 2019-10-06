#' Determines if the name is a valid distribution name
#' @param name the name to be tested
#' @return TRUE if the name is a valid distribution name, FALSE otherwise
#' @examples
#'   testit::assert(beautier:::is_distr_name("uniform"))
#'   testit::assert(beautier:::is_distr_name("normal"))
#'   testit::assert(beautier:::is_distr_name("one_div_x"))
#'   testit::assert(beautier:::is_distr_name("log_normal"))
#'   testit::assert(beautier:::is_distr_name("exponential"))
#'   testit::assert(beautier:::is_distr_name("gamma"))
#'   testit::assert(beautier:::is_distr_name("beta"))
#'   testit::assert(beautier:::is_distr_name("laplace"))
#'   testit::assert(beautier:::is_distr_name("inv_gamma"))
#'   testit::assert(beautier:::is_distr_name("poisson"))
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_distr_name <- function(name) {
  name %in% beautier::get_distr_names()
}
