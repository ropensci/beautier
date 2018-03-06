#' Initializes all site models
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized site models
#' @author Richel J.C. Bilderbeek
init_gamma_site_model <- function(
  gamma_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(is_gamma_site_model(gamma_site_model))

  if (!is_init_distr(gamma_site_model$gamma_shape_prior_distr)) {
    gamma_site_model$gamma_shape_prior_distr <- init_distr(
      gamma_site_model$gamma_shape_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + get_distr_n_params(
      gamma_site_model$gamma_shape_prior_distr
    )
  }
  testit::assert(is_gamma_site_model(gamma_site_model))
  gamma_site_model
}
