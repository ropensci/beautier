#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of distributions a site model has
#' @author Richel J.C. Bilderbeek
#' @export
get_site_model_n_params <- function(
  site_model
) {
  if (!beautier::is_site_model(site_model)) {
    stop("'site_model' must be a site model")
  }
  if (is_gtr_site_model(site_model)) {
    return(
      get_distr_n_params(site_model$rate_ac_prior_distr) +
      get_distr_n_params(site_model$rate_ag_prior_distr) +
      get_distr_n_params(site_model$rate_at_prior_distr) +
      get_distr_n_params(site_model$rate_cg_prior_distr) +
      get_distr_n_params(site_model$rate_gt_prior_distr)
    )
  } else if (is_hky_site_model(site_model)) {
    return(
      get_distr_n_params(site_model$kappa_prior_distr)
    )
  } else if (is_jc69_site_model(site_model)) {
    return(0)
  } else {
    testit::assert(beautier::is_tn93_site_model(site_model))
    return(
      get_distr_n_params(site_model$kappa_1_prior_distr) +
      get_distr_n_params(site_model$kappa_2_prior_distr)
    )
  }
}
