#' Determines if the name is a valid distribution name
#' @param name the name to be tested
#' @return TRUE if the name is a valid distribution name, FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_distr_name("uniform")
#' is_distr_name("normal")
#' is_distr_name("one_div_x")
#' is_distr_name("log_normal")
#' is_distr_name("exponential")
#' is_distr_name("gamma")
#' is_distr_name("beta")
#' is_distr_name("laplace")
#' is_distr_name("inv_gamma")
#' is_distr_name("poisson")
#' # FALSE
#' is_distr_name("nonsense")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
is_distr_name <- function(name) {
  name %in% beautier::get_distr_names()
}
