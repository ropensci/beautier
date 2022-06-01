#' Get the distribution names
#' @return the distribution names
#' @examples
#' check_empty_beautier_folder()
#'
#' get_distr_names()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_distr_names <- function() {
  c(
    "uniform", "normal", "one_div_x", "log_normal", "exponential",
    "gamma", "beta", "laplace", "inv_gamma", "poisson"
  )
}
