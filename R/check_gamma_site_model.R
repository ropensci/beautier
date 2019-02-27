#' Checks if the parameter is a valid gamma site model
#' @param x the object to be determined if it is a valid gamma site object
#' @return nothing. Will call \code{stop} if the argument is not a valid
#'   gamma site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   testthat::expect_silent(
#'     beautier:::check_gamma_site_model(
#'       create_gamma_site_model()
#'     )
#'   )
#'   testthat::expect_error(
#'     beautier:::check_gamma_site_model(
#'       "not a gamma site model"
#'     )
#'   )
#' @noRd
check_gamma_site_model <- function(gamma_site_model) {

  argument_names <- c(
    "gamma_cat_count", "gamma_shape", "prop_invariant",
    "gamma_shape_prior_distr"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(gamma_site_model)) {
      stop(
        "'", arg_name, "' must be an element of a 'gamma_site_model'. ",
        "Tip: use 'create_gamma_site_model'"
      )
    }
  }

  if (length(gamma_site_model$gamma_cat_count) != 1) {
    stop("'gamma_cat_count' must be one number")
  }
  if (gamma_site_model$gamma_cat_count < 0) {
    stop("'gamma_cat_count' must be zero or positive")
  }
  if (length(gamma_site_model$gamma_shape) != 1) {
    stop("'gamma_shape' must be one number")
  }
  if (gamma_site_model$gamma_shape < 0) {
    stop("'gamma_shape' must be zero or positive")
  }
  if (length(gamma_site_model$prop_invariant) != 1) {
    stop("'prop_invariant' must be one number")
  }
  if (gamma_site_model$prop_invariant < 0.0) {
    stop("'prop_invariant' must at least be zero")
  }
  if (gamma_site_model$prop_invariant > 1.0) {
    stop("'prop_invariant' must at most be one")
  }
  if (!is_one_na(gamma_site_model$gamma_shape_prior_distr) && # nolint beautier function
      !is_distr(gamma_site_model$gamma_shape_prior_distr)) {
    stop("'gamma_site_model' must be NA or one distribution")
  }
}
