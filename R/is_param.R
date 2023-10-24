#' Determine if the object is a valid parameter
#' @param x an object, to be determined if it is a valid parameter,
#'   as created by \code{\link{create_param}})
#' @return TRUE if x is a valid parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_param(create_alpha_param())
#' is_param(create_beta_param())
#' is_param(create_clock_rate_param())
#' is_param(create_kappa_1_param())
#' is_param(create_kappa_2_param())
#' is_param(create_lambda_param())
#' is_param(create_m_param())
#' is_param(create_mean_param())
#' is_param(create_mu_param())
#' is_param(create_rate_ac_param())
#' is_param(create_rate_ag_param())
#' is_param(create_rate_at_param())
#' is_param(create_rate_cg_param())
#' is_param(create_rate_ct_param())
#' is_param(create_rate_gt_param())
#' is_param(create_s_param())
#' is_param(create_scale_param())
#' is_param(create_sigma_param())
#'
#' # FALSE
#' is_param(NA)
#' is_param(NULL)
#' is_param("nonsense")
#' is_param(create_jc69_site_model())
#' is_param(create_strict_clock_model())
#' is_param(create_yule_tree_prior())
#' is_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_param <- function(
  x
) {
  result <- FALSE
  tryCatch({
    check_param(x)
    result <- TRUE
  },
    error = function(e) {} # nolint do not care about e
  )
  result
}

#' Determine if the object is a valid
#' alpha parameter
#' @param x an object, to be determined if it is a valid
#'   alpha parameter
#' @return TRUE if x is a valid alpha parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_alpha_param(create_alpha_param())
#' is_alpha_param(create_beta_param())
#' is_alpha_param(create_clock_rate_param())
#' is_alpha_param(create_kappa_1_param())
#' is_alpha_param(create_kappa_2_param())
#' is_alpha_param(create_lambda_param())
#' is_alpha_param(create_m_param())
#' is_alpha_param(create_mean_param())
#' is_alpha_param(create_mu_param())
#' is_alpha_param(create_rate_ac_param())
#' is_alpha_param(create_rate_ag_param())
#' is_alpha_param(create_rate_at_param())
#' is_alpha_param(create_rate_cg_param())
#' is_alpha_param(create_rate_ct_param())
#' is_alpha_param(create_rate_gt_param())
#' is_alpha_param(create_s_param())
#' is_alpha_param(create_scale_param())
#' is_alpha_param(create_sigma_param())
#'
#' is_alpha_param(NA)
#' is_alpha_param(NULL)
#' is_alpha_param("nonsense")
#' is_alpha_param(create_jc69_site_model())
#' is_alpha_param(create_strict_clock_model())
#' is_alpha_param(create_yule_tree_prior())
#' is_alpha_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_alpha_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "alpha"
}

#' Determine if the object is a valid
#' b_pop_sizes parameter
#' @param x an object, to be determined if it is a valid
#'   b_pop_sizes parameter
#' @return TRUE if x is a valid b_pop_sizes parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_b_pop_sizes_param(create_alpha_param())
#' is_b_pop_sizes_param(create_b_pop_sizes_param())
#' is_b_pop_sizes_param(create_beta_param())
#' is_b_pop_sizes_param(create_clock_rate_param())
#' is_b_pop_sizes_param(create_kappa_1_param())
#' is_b_pop_sizes_param(create_kappa_2_param())
#' is_b_pop_sizes_param(create_lambda_param())
#' is_b_pop_sizes_param(create_m_param())
#' is_b_pop_sizes_param(create_mean_param())
#' is_b_pop_sizes_param(create_mu_param())
#' is_b_pop_sizes_param(create_rate_ac_param())
#' is_b_pop_sizes_param(create_rate_ag_param())
#' is_b_pop_sizes_param(create_rate_at_param())
#' is_b_pop_sizes_param(create_rate_cg_param())
#' is_b_pop_sizes_param(create_rate_ct_param())
#' is_b_pop_sizes_param(create_rate_gt_param())
#' is_b_pop_sizes_param(create_s_param())
#' is_b_pop_sizes_param(create_scale_param())
#' is_b_pop_sizes_param(create_sigma_param())
#'
#' is_b_pop_sizes_param(NA)
#' is_b_pop_sizes_param(NULL)
#' is_b_pop_sizes_param("nonsense")
#' is_b_pop_sizes_param(create_jc69_site_model())
#' is_b_pop_sizes_param(create_strict_clock_model())
#' is_b_pop_sizes_param(create_yule_tree_prior())
#' is_b_pop_sizes_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_b_pop_sizes_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "b_pop_sizes"
}


#' Determine if the object is a valid
#' beta parameter
#' @param x an object, to be determined if it is a valid
#'   beta parameter
#' @return TRUE if x is a valid beta parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_beta_param(create_alpha_param())
#' is_beta_param(create_beta_param())
#' is_beta_param(create_clock_rate_param())
#' is_beta_param(create_kappa_1_param())
#' is_beta_param(create_kappa_2_param())
#' is_beta_param(create_lambda_param())
#' is_beta_param(create_m_param())
#' is_beta_param(create_mean_param())
#' is_beta_param(create_mu_param())
#' is_beta_param(create_rate_ac_param())
#' is_beta_param(create_rate_ag_param())
#' is_beta_param(create_rate_at_param())
#' is_beta_param(create_rate_cg_param())
#' is_beta_param(create_rate_ct_param())
#' is_beta_param(create_rate_gt_param())
#' is_beta_param(create_s_param())
#' is_beta_param(create_scale_param())
#' is_beta_param(create_sigma_param())
#'
#' is_beta_param(NA)
#' is_beta_param(NULL)
#' is_beta_param("nonsense")
#' is_beta_param(create_jc69_site_model())
#' is_beta_param(create_strict_clock_model())
#' is_beta_param(create_yule_tree_prior())
#' is_beta_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_beta_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "beta"
}

#' Determine if the object is a valid
#' clock_rate parameter
#' @param x an object, to be determined if it is a valid
#'   clock_rate parameter
#' @return TRUE if x is a valid clock_rate parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_clock_rate_param(create_alpha_param())
#' is_clock_rate_param(create_beta_param())
#' is_clock_rate_param(create_clock_rate_param())
#' is_clock_rate_param(create_kappa_1_param())
#' is_clock_rate_param(create_kappa_2_param())
#' is_clock_rate_param(create_lambda_param())
#' is_clock_rate_param(create_m_param())
#' is_clock_rate_param(create_mean_param())
#' is_clock_rate_param(create_mu_param())
#' is_clock_rate_param(create_rate_ac_param())
#' is_clock_rate_param(create_rate_ag_param())
#' is_clock_rate_param(create_rate_at_param())
#' is_clock_rate_param(create_rate_cg_param())
#' is_clock_rate_param(create_rate_ct_param())
#' is_clock_rate_param(create_rate_gt_param())
#' is_clock_rate_param(create_s_param())
#' is_clock_rate_param(create_scale_param())
#' is_clock_rate_param(create_sigma_param())
#'
#' is_clock_rate_param(NA)
#' is_clock_rate_param(NULL)
#' is_clock_rate_param("nonsense")
#' is_clock_rate_param(create_jc69_site_model())
#' is_clock_rate_param(create_strict_clock_model())
#' is_clock_rate_param(create_yule_tree_prior())
#' is_clock_rate_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_clock_rate_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "clock_rate"
}


#' Determine if the object is a valid freq parameter
#' @param x an object, to be determined if it is a valid
#'   freq parameter
#' @return TRUE if x is a valid freq parameter,
#'   FALSE otherwise
#' @seealso freq parameters are returned by
#'   \code{\link{create_freq_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_freq_param(create_alpha_param())
#' is_freq_param(create_beta_param())
#' is_freq_param(create_clock_rate_param())
#' is_freq_param(create_freq_param())
#' is_freq_param(create_freq_param())
#' is_freq_param(create_kappa_param())
#' is_freq_param(create_kappa_1_param())
#' is_freq_param(create_kappa_2_param())
#' is_freq_param(create_lambda_param())
#' is_freq_param(create_m_param())
#' is_freq_param(create_mean_param())
#' is_freq_param(create_mu_param())
#' is_freq_param(create_rate_ac_param())
#' is_freq_param(create_rate_ag_param())
#' is_freq_param(create_rate_at_param())
#' is_freq_param(create_rate_cg_param())
#' is_freq_param(create_rate_ct_param())
#' is_freq_param(create_rate_gt_param())
#' is_freq_param(create_s_param())
#' is_freq_param(create_scale_param())
#' is_freq_param(create_sigma_param())
#'
#' is_freq_param(NA)
#' is_freq_param(NULL)
#' is_freq_param("nonsense")
#' is_freq_param(create_jc69_site_model())
#' is_freq_param(create_strict_clock_model())
#' is_freq_param(create_yule_tree_prior())
#' is_freq_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_freq_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  if (x$name != "freqParameter") return(FALSE) # redundant name
  if (!"lower" %in% names(x)) return(FALSE)
  if (!"upper" %in% names(x)) return(FALSE)
  if (!"value" %in% names(x)) return(FALSE)
  if (!"estimate" %in% names(x)) return(FALSE)
  if (!"dimension" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid kappa parameter
#' @param x an object, to be determined if it is a valid
#'   kappa parameter
#' @return TRUE if x is a valid kappa parameter,
#'   FALSE otherwise
#' @seealso kappa parameters are returned by
#'   \code{\link{create_kappa_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_kappa_param(create_alpha_param())
#' is_kappa_param(create_beta_param())
#' is_kappa_param(create_clock_rate_param())
#' is_kappa_param(create_kappa_param())
#' is_kappa_param(create_kappa_1_param())
#' is_kappa_param(create_kappa_2_param())
#' is_kappa_param(create_lambda_param())
#' is_kappa_param(create_m_param())
#' is_kappa_param(create_mean_param())
#' is_kappa_param(create_mu_param())
#' is_kappa_param(create_rate_ac_param())
#' is_kappa_param(create_rate_ag_param())
#' is_kappa_param(create_rate_at_param())
#' is_kappa_param(create_rate_cg_param())
#' is_kappa_param(create_rate_ct_param())
#' is_kappa_param(create_rate_gt_param())
#' is_kappa_param(create_s_param())
#' is_kappa_param(create_scale_param())
#' is_kappa_param(create_sigma_param())
#'
#' is_kappa_param(NA)
#' is_kappa_param(NULL)
#' is_kappa_param("nonsense")
#' is_kappa_param(create_jc69_site_model())
#' is_kappa_param(create_strict_clock_model())
#' is_kappa_param(create_yule_tree_prior())
#' is_kappa_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_kappa_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  if (x$name != "kappa") return(FALSE)

  if (!"lower" %in% names(x)) return(FALSE)
  if (!"estimate" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid kappa 1 parameter
#' @param x an object, to be determined if it is a valid
#'   kappa 1 parameter
#' @return TRUE if x is a valid kappa 1 parameter,
#'   FALSE otherwise
#' @seealso kappa 1 parameters are returned by
#'   \code{\link{create_kappa_1_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_kappa_1_param(create_alpha_param())
#' is_kappa_1_param(create_beta_param())
#' is_kappa_1_param(create_clock_rate_param())
#' is_kappa_1_param(create_kappa_param())
#' is_kappa_1_param(create_kappa_1_param())
#' is_kappa_1_param(create_kappa_2_param())
#' is_kappa_1_param(create_lambda_param())
#' is_kappa_1_param(create_m_param())
#' is_kappa_1_param(create_mean_param())
#' is_kappa_1_param(create_mu_param())
#' is_kappa_1_param(create_rate_ac_param())
#' is_kappa_1_param(create_rate_ag_param())
#' is_kappa_1_param(create_rate_at_param())
#' is_kappa_1_param(create_rate_cg_param())
#' is_kappa_1_param(create_rate_ct_param())
#' is_kappa_1_param(create_rate_gt_param())
#' is_kappa_1_param(create_s_param())
#' is_kappa_1_param(create_scale_param())
#' is_kappa_1_param(create_sigma_param())
#'
#' is_kappa_1_param(NA)
#' is_kappa_1_param(NULL)
#' is_kappa_1_param("nonsense")
#' is_kappa_1_param(create_jc69_site_model())
#' is_kappa_1_param(create_strict_clock_model())
#' is_kappa_1_param(create_yule_tree_prior())
#' is_kappa_1_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_kappa_1_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  if (x$name != "kappa_1") return(FALSE)

  if (!"lower" %in% names(x)) return(FALSE)
  if (!"estimate" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid kappa 2 parameter
#' @param x an object, to be determined if it is a valid
#'   kappa 2 parameter
#' @return TRUE if x is a valid kappa_2 parameter,
#'   FALSE otherwise
#' @seealso kappa 2 parameters are returned by
#'   \code{\link{create_kappa_2_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_kappa_2_param(create_alpha_param())
#' is_kappa_2_param(create_beta_param())
#' is_kappa_2_param(create_clock_rate_param())
#' is_kappa_2_param(create_kappa_1_param())
#' is_kappa_2_param(create_kappa_2_param())
#' is_kappa_2_param(create_lambda_param())
#' is_kappa_2_param(create_m_param())
#' is_kappa_2_param(create_mean_param())
#' is_kappa_2_param(create_mu_param())
#' is_kappa_2_param(create_rate_ac_param())
#' is_kappa_2_param(create_rate_ag_param())
#' is_kappa_2_param(create_rate_at_param())
#' is_kappa_2_param(create_rate_cg_param())
#' is_kappa_2_param(create_rate_ct_param())
#' is_kappa_2_param(create_rate_gt_param())
#' is_kappa_2_param(create_s_param())
#' is_kappa_2_param(create_scale_param())
#' is_kappa_2_param(create_sigma_param())
#'
#' is_kappa_2_param(NA)
#' is_kappa_2_param(NULL)
#' is_kappa_2_param("nonsense")
#' is_kappa_2_param(create_jc69_site_model())
#' is_kappa_2_param(create_strict_clock_model())
#' is_kappa_2_param(create_yule_tree_prior())
#' is_kappa_2_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_kappa_2_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  if (x$name != "kappa_2") return(FALSE)

  if (!"lower" %in% names(x)) return(FALSE)
  if (!"estimate" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid lambda parameter
#' @param x an object, to be determined if it is a valid
#'   lambda parameter
#' @return TRUE if x is a valid lambda parameter,
#'   FALSE otherwise
#' @seealso lambda parameters are returned by \code{\link{create_lambda_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_lambda_param(create_alpha_param())
#' is_lambda_param(create_beta_param())
#' is_lambda_param(create_clock_rate_param())
#' is_lambda_param(create_kappa_1_param())
#' is_lambda_param(create_kappa_2_param())
#' is_lambda_param(create_lambda_param())
#' is_lambda_param(create_m_param())
#' is_lambda_param(create_mean_param())
#' is_lambda_param(create_mu_param())
#' is_lambda_param(create_rate_ac_param())
#' is_lambda_param(create_rate_ag_param())
#' is_lambda_param(create_rate_at_param())
#' is_lambda_param(create_rate_cg_param())
#' is_lambda_param(create_rate_ct_param())
#' is_lambda_param(create_rate_gt_param())
#' is_lambda_param(create_s_param())
#' is_lambda_param(create_scale_param())
#' is_lambda_param(create_sigma_param())
#'
#' is_lambda_param(NA)
#' is_lambda_param(NULL)
#' is_lambda_param("nonsense")
#' is_lambda_param(create_jc69_site_model())
#' is_lambda_param(create_strict_clock_model())
#' is_lambda_param(create_yule_tree_prior())
#' is_lambda_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_lambda_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "lambda"
}

#' Determine if the object is a valid mean parameter
#' @param x an object, to be determined if it is a valid mean parameter,
#'   as created by \code{\link{create_mean_param}})
#' @return TRUE if x is a valid mean parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_mean_param(create_alpha_param())
#' is_mean_param(create_beta_param())
#' is_mean_param(create_clock_rate_param())
#' is_mean_param(create_kappa_1_param())
#' is_mean_param(create_kappa_2_param())
#' is_mean_param(create_lambda_param())
#' is_mean_param(create_m_param())
#' is_mean_param(create_mean_param())
#' is_mean_param(create_mu_param())
#' is_mean_param(create_rate_ac_param())
#' is_mean_param(create_rate_ag_param())
#' is_mean_param(create_rate_at_param())
#' is_mean_param(create_rate_cg_param())
#' is_mean_param(create_rate_ct_param())
#' is_mean_param(create_rate_gt_param())
#' is_mean_param(create_s_param())
#' is_mean_param(create_scale_param())
#' is_mean_param(create_sigma_param())
#'
#' is_mean_param(NA)
#' is_mean_param(NULL)
#' is_mean_param("nonsense")
#' is_mean_param(create_jc69_site_model())
#' is_mean_param(create_strict_clock_model())
#' is_mean_param(create_yule_tree_prior())
#' is_mean_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_mean_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "mean"
}

#' Determine if the object is a valid
#' mu parameter
#' @param x an object, to be determined if it is a valid
#'   mu parameter
#' @return TRUE if x is a valid mu parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_mu_param}} creates a mu parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_mu_param(create_alpha_param())
#' is_mu_param(create_beta_param())
#' is_mu_param(create_clock_rate_param())
#' is_mu_param(create_kappa_1_param())
#' is_mu_param(create_kappa_2_param())
#' is_mu_param(create_lambda_param())
#' is_mu_param(create_m_param())
#' is_mu_param(create_mean_param())
#' is_mu_param(create_mu_param())
#' is_mu_param(create_rate_ac_param())
#' is_mu_param(create_rate_ag_param())
#' is_mu_param(create_rate_at_param())
#' is_mu_param(create_rate_cg_param())
#' is_mu_param(create_rate_ct_param())
#' is_mu_param(create_rate_gt_param())
#' is_mu_param(create_s_param())
#' is_mu_param(create_scale_param())
#' is_mu_param(create_sigma_param())
#'
#' is_mu_param(NA)
#' is_mu_param(NULL)
#' is_mu_param("nonsense")
#' is_mu_param(create_jc69_site_model())
#' is_mu_param(create_strict_clock_model())
#' is_mu_param(create_yule_tree_prior())
#' is_mu_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_mu_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "mu"
}

#' Determine if the object is a valid
#' 'rate AC' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AC' parameter
#' @return TRUE if x is a valid 'rate AC' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ac_param}} creates a 'rate AC' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_rate_ac_param(create_alpha_param())
#' is_rate_ac_param(create_beta_param())
#' is_rate_ac_param(create_clock_rate_param())
#' is_rate_ac_param(create_kappa_1_param())
#' is_rate_ac_param(create_kappa_2_param())
#' is_rate_ac_param(create_lambda_param())
#' is_rate_ac_param(create_m_param())
#' is_rate_ac_param(create_mean_param())
#' is_rate_ac_param(create_mu_param())
#' is_rate_ac_param(create_rate_ac_param())
#' is_rate_ac_param(create_rate_ag_param())
#' is_rate_ac_param(create_rate_at_param())
#' is_rate_ac_param(create_rate_cg_param())
#' is_rate_ac_param(create_rate_ct_param())
#' is_rate_ac_param(create_rate_gt_param())
#' is_rate_ac_param(create_s_param())
#' is_rate_ac_param(create_scale_param())
#' is_rate_ac_param(create_sigma_param())
#'
#' is_rate_ac_param(NA)
#' is_rate_ac_param(NULL)
#' is_rate_ac_param("nonsense")
#' is_rate_ac_param(create_jc69_site_model())
#' is_rate_ac_param(create_strict_clock_model())
#' is_rate_ac_param(create_yule_tree_prior())
#' is_rate_ac_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_rate_ac_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_ac"
}

#' Determine if the object is a valid
#' 'rate AG' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AG' parameter
#' @return TRUE if x is a valid 'rate AG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ag_param}} creates a 'rate AG' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_rate_ag_param(create_alpha_param())
#' is_rate_ag_param(create_beta_param())
#' is_rate_ag_param(create_clock_rate_param())
#' is_rate_ag_param(create_kappa_1_param())
#' is_rate_ag_param(create_kappa_2_param())
#' is_rate_ag_param(create_lambda_param())
#' is_rate_ag_param(create_m_param())
#' is_rate_ag_param(create_mean_param())
#' is_rate_ag_param(create_mu_param())
#' is_rate_ag_param(create_rate_ac_param())
#' is_rate_ag_param(create_rate_ag_param())
#' is_rate_ag_param(create_rate_at_param())
#' is_rate_ag_param(create_rate_cg_param())
#' is_rate_ag_param(create_rate_ct_param())
#' is_rate_ag_param(create_rate_gt_param())
#' is_rate_ag_param(create_s_param())
#' is_rate_ag_param(create_scale_param())
#' is_rate_ag_param(create_sigma_param())
#'
#' is_rate_ag_param(NA)
#' is_rate_ag_param(NULL)
#' is_rate_ag_param("nonsense")
#' is_rate_ag_param(create_jc69_site_model())
#' is_rate_ag_param(create_strict_clock_model())
#' is_rate_ag_param(create_yule_tree_prior())
#' is_rate_ag_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_rate_ag_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_ag"
}

#' Determine if the object is a valid
#' 'rate AT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AT' parameter
#' @return TRUE if x is a valid 'rate AT' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_at_param}} creates a 'rate AT' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_rate_at_param(create_alpha_param())
#' is_rate_at_param(create_beta_param())
#' is_rate_at_param(create_clock_rate_param())
#' is_rate_at_param(create_kappa_1_param())
#' is_rate_at_param(create_kappa_2_param())
#' is_rate_at_param(create_lambda_param())
#' is_rate_at_param(create_m_param())
#' is_rate_at_param(create_mean_param())
#' is_rate_at_param(create_mu_param())
#' is_rate_at_param(create_rate_ac_param())
#' is_rate_at_param(create_rate_ag_param())
#' is_rate_at_param(create_rate_at_param())
#' is_rate_at_param(create_rate_cg_param())
#' is_rate_at_param(create_rate_ct_param())
#' is_rate_at_param(create_rate_gt_param())
#' is_rate_at_param(create_s_param())
#' is_rate_at_param(create_scale_param())
#' is_rate_at_param(create_sigma_param())
#'
#' is_rate_at_param(NA)
#' is_rate_at_param(NULL)
#' is_rate_at_param("nonsense")
#' is_rate_at_param(create_jc69_site_model())
#' is_rate_at_param(create_strict_clock_model())
#' is_rate_at_param(create_yule_tree_prior())
#' is_rate_at_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_rate_at_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_at"
}

#' Determine if the object is a valid
#' 'rate CG' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate CG' parameter
#' @return TRUE if x is a valid 'rate CG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_cg_param}} creates a 'rate CG' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_rate_cg_param(create_alpha_param())
#' is_rate_cg_param(create_beta_param())
#' is_rate_cg_param(create_clock_rate_param())
#' is_rate_cg_param(create_kappa_1_param())
#' is_rate_cg_param(create_kappa_2_param())
#' is_rate_cg_param(create_lambda_param())
#' is_rate_cg_param(create_m_param())
#' is_rate_cg_param(create_mean_param())
#' is_rate_cg_param(create_mu_param())
#' is_rate_cg_param(create_rate_ac_param())
#' is_rate_cg_param(create_rate_ag_param())
#' is_rate_cg_param(create_rate_at_param())
#' is_rate_cg_param(create_rate_cg_param())
#' is_rate_cg_param(create_rate_ct_param())
#' is_rate_cg_param(create_rate_gt_param())
#' is_rate_cg_param(create_s_param())
#' is_rate_cg_param(create_scale_param())
#' is_rate_cg_param(create_sigma_param())
#'
#' is_rate_cg_param(NA)
#' is_rate_cg_param(NULL)
#' is_rate_cg_param("nonsense")
#' is_rate_cg_param(create_jc69_site_model())
#' is_rate_cg_param(create_strict_clock_model())
#' is_rate_cg_param(create_yule_tree_prior())
#' is_rate_cg_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_rate_cg_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_cg"
}

#' Determine if the object is a valid
#' 'rate CT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate CT' parameter
#' @return TRUE if x is a valid 'rate CG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ct_param}} creates a 'rate CT' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_rate_ct_param(create_alpha_param())
#' is_rate_ct_param(create_beta_param())
#' is_rate_ct_param(create_clock_rate_param())
#' is_rate_ct_param(create_kappa_1_param())
#' is_rate_ct_param(create_kappa_2_param())
#' is_rate_ct_param(create_lambda_param())
#' is_rate_ct_param(create_m_param())
#' is_rate_ct_param(create_mean_param())
#' is_rate_ct_param(create_mu_param())
#' is_rate_ct_param(create_rate_ac_param())
#' is_rate_ct_param(create_rate_ag_param())
#' is_rate_ct_param(create_rate_at_param())
#' is_rate_ct_param(create_rate_cg_param())
#' is_rate_ct_param(create_rate_ct_param())
#' is_rate_ct_param(create_rate_gt_param())
#' is_rate_ct_param(create_s_param())
#' is_rate_ct_param(create_scale_param())
#' is_rate_ct_param(create_sigma_param())
#'
#' is_rate_ct_param(NA)
#' is_rate_ct_param(NULL)
#' is_rate_ct_param("nonsense")
#' is_rate_ct_param(create_jc69_site_model())
#' is_rate_ct_param(create_strict_clock_model())
#' is_rate_ct_param(create_yule_tree_prior())
#' is_rate_ct_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_rate_ct_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_ct"
}

#' Determine if the object is a valid
#' 'rate GT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate GT' parameter
#' @return TRUE if x is a valid 'rate GT' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_gt_param}} creates a 'rate GT' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_rate_gt_param(create_alpha_param())
#' is_rate_gt_param(create_beta_param())
#' is_rate_gt_param(create_clock_rate_param())
#' is_rate_gt_param(create_kappa_1_param())
#' is_rate_gt_param(create_kappa_2_param())
#' is_rate_gt_param(create_lambda_param())
#' is_rate_gt_param(create_m_param())
#' is_rate_gt_param(create_mean_param())
#' is_rate_gt_param(create_mu_param())
#' is_rate_gt_param(create_rate_ac_param())
#' is_rate_gt_param(create_rate_ag_param())
#' is_rate_gt_param(create_rate_at_param())
#' is_rate_gt_param(create_rate_cg_param())
#' is_rate_gt_param(create_rate_ct_param())
#' is_rate_gt_param(create_rate_gt_param())
#' is_rate_gt_param(create_s_param())
#' is_rate_gt_param(create_scale_param())
#' is_rate_gt_param(create_sigma_param())
#'
#' is_rate_gt_param(NA)
#' is_rate_gt_param(NULL)
#' is_rate_gt_param("nonsense")
#' is_rate_gt_param(create_jc69_site_model())
#' is_rate_gt_param(create_strict_clock_model())
#' is_rate_gt_param(create_yule_tree_prior())
#' is_rate_gt_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_rate_gt_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_gt"
}

#' Determine if the object is a valid
#' s parameter
#' @param x an object, to be determined if it is a valid
#'   s parameter
#' @return TRUE if x is a valid s parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_s_param(create_alpha_param())
#' is_s_param(create_beta_param())
#' is_s_param(create_clock_rate_param())
#' is_s_param(create_kappa_1_param())
#' is_s_param(create_kappa_2_param())
#' is_s_param(create_lambda_param())
#' is_s_param(create_m_param())
#' is_s_param(create_mean_param())
#' is_s_param(create_mu_param())
#' is_s_param(create_rate_ac_param())
#' is_s_param(create_rate_ag_param())
#' is_s_param(create_rate_at_param())
#' is_s_param(create_rate_cg_param())
#' is_s_param(create_rate_ct_param())
#' is_s_param(create_rate_gt_param())
#' is_s_param(create_s_param())
#' is_s_param(create_scale_param())
#' is_s_param(create_sigma_param())
#'
#' is_s_param(NA)
#' is_s_param(NULL)
#' is_s_param("nonsense")
#' is_s_param(create_jc69_site_model())
#' is_s_param(create_strict_clock_model())
#' is_s_param(create_yule_tree_prior())
#' is_s_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_s_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "s"
}

#' Determine if the object is a valid
#' scale parameter
#' @param x an object, to be determined if it is a valid
#'   scale parameter
#' @return TRUE if x is a valid scale parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_scale_param(create_alpha_param())
#' is_scale_param(create_beta_param())
#' is_scale_param(create_clock_rate_param())
#' is_scale_param(create_kappa_1_param())
#' is_scale_param(create_kappa_2_param())
#' is_scale_param(create_lambda_param())
#' is_scale_param(create_m_param())
#' is_scale_param(create_mean_param())
#' is_scale_param(create_mu_param())
#' is_scale_param(create_rate_ac_param())
#' is_scale_param(create_rate_ag_param())
#' is_scale_param(create_rate_at_param())
#' is_scale_param(create_rate_cg_param())
#' is_scale_param(create_rate_ct_param())
#' is_scale_param(create_rate_gt_param())
#' is_scale_param(create_s_param())
#' is_scale_param(create_scale_param())
#' is_scale_param(create_sigma_param())
#'
#' is_scale_param(NA)
#' is_scale_param(NULL)
#' is_scale_param("nonsense")
#' is_scale_param(create_jc69_site_model())
#' is_scale_param(create_strict_clock_model())
#' is_scale_param(create_yule_tree_prior())
#' is_scale_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_scale_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "scale"
}

#' Determine if the object is a valid
#' sigma parameter
#' @param x an object, to be determined if it is a valid
#'   sigma parameter
#' @return TRUE if x is a valid sigma parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_sigma_param(create_alpha_param())
#' is_sigma_param(create_beta_param())
#' is_sigma_param(create_clock_rate_param())
#' is_sigma_param(create_kappa_1_param())
#' is_sigma_param(create_kappa_2_param())
#' is_sigma_param(create_lambda_param())
#' is_sigma_param(create_m_param())
#' is_sigma_param(create_mean_param())
#' is_sigma_param(create_mu_param())
#' is_sigma_param(create_rate_ac_param())
#' is_sigma_param(create_rate_ag_param())
#' is_sigma_param(create_rate_at_param())
#' is_sigma_param(create_rate_cg_param())
#' is_sigma_param(create_rate_ct_param())
#' is_sigma_param(create_rate_gt_param())
#' is_sigma_param(create_s_param())
#' is_sigma_param(create_scale_param())
#' is_sigma_param(create_sigma_param())
#'
#' is_sigma_param(NA)
#' is_sigma_param(NULL)
#' is_sigma_param("nonsense")
#' is_sigma_param(create_jc69_site_model())
#' is_sigma_param(create_strict_clock_model())
#' is_sigma_param(create_yule_tree_prior())
#' is_sigma_param(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_sigma_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "sigma"
}
