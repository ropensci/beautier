#' Check if the \code{tn93_site_model} is a valid
#' TN93 nucleotide substitution model.
#'
#' Use \link{create_tn93_site_model} to create a valid
#' TN93 nucleotide substitution model.
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' expect_silent(check_tn93_site_model(create_tn93_site_model()))
#'
#' expect_error(check_tn93_site_model("nonsense"))
#' expect_error(check_tn93_site_model(NA))
#' expect_error(check_tn93_site_model(NULL))
#' expect_error(check_tn93_site_model(""))
#' expect_error(check_tn93_site_model(c()))
#' @export
check_tn93_site_model <- function(tn93_site_model) {

  if (!beautier::is_site_model(tn93_site_model)) {
    stop("'tn93_site_model' must be a site model")
  }
  beautier::check_tn93_site_model_names(tn93_site_model)

  if (!beautier::is_distr(tn93_site_model$kappa_1_prior_distr)) {
    stop("'tn93_site_model$kappa_1_prior_distr' must be a distribution")
  }
  if (!beautier::is_distr(tn93_site_model$kappa_2_prior_distr)) {
    stop("'tn93_site_model$kappa_2_prior_distr' must be a distribution")
  }
  if (!beautier::is_param(tn93_site_model$kappa_1_param)) {
    stop("'tn93_site_model$kappa_1_param' must be a parameter")
  }
  if (!beautier::is_param(tn93_site_model$kappa_2_param)) {
    stop("'tn93_site_model$kappa_2_param' must be a parameter")
  }
  if (!beautier::is_freq_equilibrium_name(tn93_site_model$freq_equilibrium)) {
    stop(
      "'tn93_site_model$freq_equilibrium' must be ",
      "an equilibrium frequency name"
    )
  }

}

#' Check if the \code{tn93_site_model} has the list elements
#' of a valid \code{tn93_site_model} object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_tn93_site_model}
#' to create a valid \code{tn93_site_model}
#' @author Richèl J.C. Bilderbeek
#' @export
check_tn93_site_model_names <- function(tn93_site_model) {

  list_element_names <- c(
    "kappa_1_prior_distr", "kappa_2_prior_distr",
    "kappa_1_param", "kappa_2_param", "freq_equilibrium"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(tn93_site_model)) {
      stop(
        "'", arg_name, "' must be an element of an 'tn93_site_model'. \n",
        "Tip: use 'create_tn93_site_model'"
      )
    }
  }
}
