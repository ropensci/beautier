#' Check if the \code{gtr_site_model} is a valid
#' GTR nucleotide substitution model.
#'
#' Use \link{create_gtr_site_model} to create a valid
#' GTR nucleotide substitution model.
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' expect_silent(check_gtr_site_model(create_gtr_site_model()))
#'
#' expect_error(check_gtr_site_model("nonsense"))
#' expect_error(check_gtr_site_model(NA))
#' expect_error(check_gtr_site_model(NULL))
#' expect_error(check_gtr_site_model(""))
#' expect_error(check_gtr_site_model(c()))
#' @export
check_gtr_site_model <- function(gtr_site_model) {
  if (!beautier::is_site_model(gtr_site_model)) {
    stop("'gtr_site_model' must be a site model")
  }
  if (gtr_site_model$name != "GTR") {
    stop("'gtr_site_model$name' must be 'GTR'")
  }
  beautier::check_gtr_site_model_names(gtr_site_model)

  # Check if all distributions are valid distributions
  expected_distrs <- list(
    gtr_site_model$rate_ac_prior_distr,
    gtr_site_model$rate_ag_prior_distr,
    gtr_site_model$rate_at_prior_distr,
    gtr_site_model$rate_cg_prior_distr,
    gtr_site_model$rate_gt_prior_distr
  )
  for (expected_distr in expected_distrs) {
    if (!beautier::is_distr(expected_distr)) {
      stop("Invalid gtr_site_model$distr")
    }
  }

  # Check if all parameters are valid parameters
  expected_params <- list(
    gtr_site_model$rate_ac_param,
    gtr_site_model$rate_ag_param,
    gtr_site_model$rate_at_param,
    gtr_site_model$rate_cg_param,
    gtr_site_model$rate_ct_param,
    gtr_site_model$rate_gt_param
  )
  for (expected_param in expected_params) {
    if (!beautier::is_param(expected_param)) {
      stop("Invalid gtr_site_model$param")
    }
  }

  if (!beautier::is_freq_equilibrium_name(gtr_site_model$freq_equilibrium)) {
    stop("Invalid gtr_site_model$freq_equilibrium")
  }
  TRUE

}

#' Check if the \code{gtr_site_model} has the list elements
#' of a valid \code{gtr_site_model} object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_gtr_site_model}
#' to create a valid \code{gtr_site_model}
#' @author Richèl J.C. Bilderbeek
#' @export
check_gtr_site_model_names <- function(gtr_site_model) {

  list_element_names <- c(
    "rate_ac_prior_distr", "rate_ag_prior_distr",
    "rate_at_prior_distr", "rate_cg_prior_distr", "rate_gt_prior_distr",
    "rate_ac_param", "rate_ag_param", "rate_at_param", "rate_cg_param",
    "rate_ct_param", "rate_gt_param", "freq_equilibrium"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(gtr_site_model)) {
      stop(
        "'", arg_name, "' must be an element of an 'gtr_site_model'. \n",
        "Tip: use 'create_gtr_site_model'"
      )
    }
  }
}
