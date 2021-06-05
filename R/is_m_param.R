#' Determine if the object is a valid
#' m parameter
#' @inheritParams default_params_doc
#' @return TRUE if x is a valid m parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' is_m_param(create_alpha_param())
#' is_m_param(create_beta_param())
#' is_m_param(create_clock_rate_param())
#' is_m_param(create_kappa_1_param())
#' is_m_param(create_kappa_2_param())
#' is_m_param(create_lambda_param())
#' is_m_param(create_m_param())
#' is_m_param(create_mean_param())
#' is_m_param(create_mu_param())
#' is_m_param(create_rate_ac_param())
#' is_m_param(create_rate_ag_param())
#' is_m_param(create_rate_at_param())
#' is_m_param(create_rate_cg_param())
#' is_m_param(create_rate_ct_param())
#' is_m_param(create_rate_gt_param())
#' is_m_param(create_s_param())
#' is_m_param(create_scale_param())
#' is_m_param(create_sigma_param())
#'
#' is_m_param(NA)
#' is_m_param(NULL)
#' is_m_param("nonsense")
#' is_m_param(create_jc69_site_model())
#' is_m_param(create_strict_clock_model())
#' is_m_param(create_yule_tree_prior())
#' is_m_param(create_mcmc())
#' @export
is_m_param <- function(
  m_param
) {
  if (!beautier::is_param(m_param)) return(FALSE)
  m_param$name == "m"
}
