#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of parameters a site model has
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     beautier:::get_gamma_site_model_n_params(
#'       create_gamma_site_model(gamma_cat_count = 0)
#'     ) == 0
#'   )
#'   testit::assert(
#'     beautier:::get_gamma_site_model_n_params(
#'       create_gamma_site_model(gamma_cat_count = 1)
#'     ) == 0
#'   )
#'   testit::assert(
#'     beautier:::get_gamma_site_model_n_params(
#'       create_gamma_site_model(
#'         gamma_cat_count = 2,
#'         gamma_shape_prior_distr = create_exp_distr()
#'       )
#'     ) == 1
#'   )
#' @noRd
get_gamma_site_model_n_params <- function(
  gamma_site_model
) {
  testit::assert(is_gamma_site_model(gamma_site_model)) # nolint beautier function
  if (gamma_site_model$gamma_cat_count < 2) {
    0
  } else {
    get_distr_n_params(gamma_site_model$gamma_shape_prior_distr) # nolint beautier function
  }
}
