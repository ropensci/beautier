#' Checks if the parameter is a valid gamma site model
#' @inheritParams default_params_doc
#' @return nothing. Will call \code{stop} if the argument is not a valid
#'   gamma site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_gamma_site_model(create_gamma_site_model())
#' @export
check_gamma_site_model <- function(gamma_site_model) {

  beautier::check_gamma_site_model_names(gamma_site_model)

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
  if (!beautier::is_one_na(gamma_site_model$gamma_shape_prior_distr) &&
      !beautier::is_distr(gamma_site_model$gamma_shape_prior_distr)) {
    stop(
      "'gamma_site_model$gamma_shape_prior_distr' must be NA ",
      "or one distribution"
    )
  }
  if (gamma_site_model$gamma_cat_count < 2 &&
      !beautier::is_one_na(gamma_site_model$gamma_shape_prior_distr)) {
    stop(
      "'gamma_shape_prior_distr' must be NA ",
      "for a 'gamma_cat_count' of less than two"
    )
  }

}

#' Checks if the gamma site model has the right list elements' names
#' @inheritParams default_params_doc
#' @return nothing. Will call \code{stop} if the argument is not a valid
#'   gamma site model
#' @author Richèl J.C. Bilderbeek
#' @export
check_gamma_site_model_names <- function(gamma_site_model) {

  argument_names <- c(
    "gamma_cat_count", "gamma_shape", "prop_invariant",
    "gamma_shape_prior_distr", "freq_equilibrium"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(gamma_site_model)) {
      stop(
        "'", arg_name, "' must be an element of a 'gamma_site_model'. ",
        "Tip: use 'create_gamma_site_model'"
      )
    }
  }
}
