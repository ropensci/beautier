#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of distributions a site model has
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     beautier:::get_site_model_n_params(create_gtr_site_model()) == 11
#'   )
#'   testit::assert(
#'     beautier:::get_site_model_n_params(create_hky_site_model()) == 3
#'   )
#'   testit::assert(
#'     beautier:::get_site_model_n_params(create_jc69_site_model()) == 1
#'   )
#'   testit::assert(
#'     beautier:::get_site_model_n_params(create_tn93_site_model()) == 5
#'   )
get_site_model_n_params <- function(
  site_model
) {
  if (!is_site_model(site_model)) {
    stop("'site_model' must be a site model")
  }
  gamma_site_model_n_params <- get_distr_n_params(
    site_model$gamma_site_model$gamma_shape_prior_distr
  )

  if (is_gtr_site_model(site_model)) {
    return(
      gamma_site_model_n_params +
      get_distr_n_params(site_model$rate_ac_prior_distr) +
      get_distr_n_params(site_model$rate_ag_prior_distr) +
      get_distr_n_params(site_model$rate_at_prior_distr) +
      get_distr_n_params(site_model$rate_cg_prior_distr) +
      get_distr_n_params(site_model$rate_gt_prior_distr)
    )
  } else if (is_hky_site_model(site_model)) {
    return(
      gamma_site_model_n_params +
      get_distr_n_params(site_model$kappa_prior_distr)
    )
  } else if (is_jc69_site_model(site_model)) {
    return(
      gamma_site_model_n_params +
      0
    )
  } else {
    testit::assert(is_tn93_site_model(site_model))
    return(
      gamma_site_model_n_params +
      get_distr_n_params(site_model$kappa_1_prior_distr) +
      get_distr_n_params(site_model$kappa_2_prior_distr)
    )
  }
}
