#' Get the number of distributions in a gamma site model
#' @inheritParams default_params_doc
#' @return the number of distributions a gamma site model has
#' @seealso Use \link{create_gamma_site_model} to create a gamma site model
#' @examples
#' check_empty_beautier_folder()
#'
#' # zero distributions
#' gamma_site_model <- create_gamma_site_model()
#' get_gamma_site_model_n_distrs(
#'   gamma_site_model
#' )
#'
#' gamma_site_model <- create_gamma_site_model(
#'  gamma_cat_count = 2,
#'  gamma_shape_prior_distr = create_exp_distr()
#' )
#'
#' # one distribution
#' get_gamma_site_model_n_distrs(gamma_site_model)
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_gamma_site_model_n_distrs <- function(gamma_site_model) {
  testit::assert(beautier::is_gamma_site_model(gamma_site_model))
  if (gamma_site_model$gamma_cat_count < 2) {
    0
  } else {
    1
  }
}
