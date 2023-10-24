#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of parameters a site model has
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # Ten
#' get_site_model_n_params(create_gtr_site_model())
#'
#' # Two
#' get_site_model_n_params(create_hky_site_model())
#'
#' # Zero
#' get_site_model_n_params(create_jc69_site_model())
#'
#' # Four
#' get_site_model_n_params(create_tn93_site_model())
#'
#' check_empty_beautier_folder()
#' @export
get_site_model_n_params <- function(
  site_model
) {
  if (!is_site_model(site_model)) {
    stop("'site_model' must be a site model")
  }
  gamma_site_model_n_params <- get_gamma_site_model_n_params(
    site_model$gamma_site_model
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
    check_true(is_tn93_site_model(site_model))
    return(
      gamma_site_model_n_params +
        get_distr_n_params(site_model$kappa_1_prior_distr) +
        get_distr_n_params(site_model$kappa_2_prior_distr)
    )
  }
}
