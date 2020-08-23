#' Get the distribution names
#' @return the distribution names
#' @examples
#' get_distr_names()
#' @author Richèl J.C. Bilderbeek
#' @export
get_distr_names <- function() {
  c(
    "uniform", "normal", "one_div_x", "log_normal", "exponential",
    "gamma", "beta", "laplace", "inv_gamma", "poisson"
  )
}
