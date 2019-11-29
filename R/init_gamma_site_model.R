#' Initializes all site models
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return an initialized gamma site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   gamma_site_model <- create_gamma_site_model(
#'     gamma_shape_prior_distr = create_one_div_x_distr(id = NA)
#'   )
#'  testit::assert(!beautier:::is_init_gamma_site_model(gamma_site_model))
#'  gamma_site_model <- beautier:::init_gamma_site_model(gamma_site_model)
#'  testit::assert(beautier:::is_init_gamma_site_model(gamma_site_model))
#' @noRd
init_gamma_site_model <- function(
  gamma_site_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_gamma_site_model(gamma_site_model))

  if (!is_init_distr(gamma_site_model$gamma_shape_prior_distr)) { # nolint beautier function
    gamma_site_model$gamma_shape_prior_distr <- init_distr( # nolint beautier function
      gamma_site_model$gamma_shape_prior_distr,
      distr_id = distr_id,
      param_id = param_id
    )
    distr_id <- distr_id + 1
    param_id <- param_id + beautier::get_distr_n_params(
      gamma_site_model$gamma_shape_prior_distr
    )
  }
  testit::assert(beautier::is_gamma_site_model(gamma_site_model))
  gamma_site_model
}
