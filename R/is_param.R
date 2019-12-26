#' Determine if the object is a valid parameter
#' @param x an object, to be determined if it is a valid parameter,
#'   as created by \code{\link{create_param}})
#' @return TRUE if x is a valid parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_true(is_param(create_alpha_param()))
#' expect_true(is_param(create_beta_param()))
#' expect_true(is_param(create_clock_rate_param()))
#' expect_true(is_param(create_kappa_1_param()))
#' expect_true(is_param(create_kappa_2_param()))
#' expect_true(is_param(create_lambda_param()))
#' expect_true(is_param(create_m_param()))
#' expect_true(is_param(create_mean_param()))
#' expect_true(is_param(create_mu_param()))
#' expect_true(is_param(create_rate_ac_param()))
#' expect_true(is_param(create_rate_ag_param()))
#' expect_true(is_param(create_rate_at_param()))
#' expect_true(is_param(create_rate_cg_param()))
#' expect_true(is_param(create_rate_ct_param()))
#' expect_true(is_param(create_rate_gt_param()))
#' expect_true(is_param(create_s_param()))
#' expect_true(is_param(create_scale_param()))
#' expect_true(is_param(create_sigma_param()))
#'
#' expect_false(is_param(NA))
#' expect_false(is_param(NULL))
#' expect_false(is_param("nonsense"))
#' expect_false(is_param(create_jc69_site_model()))
#' expect_false(is_param(create_strict_clock_model()))
#' expect_false(is_param(create_yule_tree_prior()))
#' expect_false(is_param(create_mcmc()))
#' @export
is_param <- function(
  x
) {
  result <- FALSE
  tryCatch({
    beautier::check_param(x)
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
#' library(testthat)
#'
#' expect_true(is_alpha_param(create_alpha_param()))
#' expect_false(is_alpha_param(create_beta_param()))
#' expect_false(is_alpha_param(create_clock_rate_param()))
#' expect_false(is_alpha_param(create_kappa_1_param()))
#' expect_false(is_alpha_param(create_kappa_2_param()))
#' expect_false(is_alpha_param(create_lambda_param()))
#' expect_false(is_alpha_param(create_m_param()))
#' expect_false(is_alpha_param(create_mean_param()))
#' expect_false(is_alpha_param(create_mu_param()))
#' expect_false(is_alpha_param(create_rate_ac_param()))
#' expect_false(is_alpha_param(create_rate_ag_param()))
#' expect_false(is_alpha_param(create_rate_at_param()))
#' expect_false(is_alpha_param(create_rate_cg_param()))
#' expect_false(is_alpha_param(create_rate_ct_param()))
#' expect_false(is_alpha_param(create_rate_gt_param()))
#' expect_false(is_alpha_param(create_s_param()))
#' expect_false(is_alpha_param(create_scale_param()))
#' expect_false(is_alpha_param(create_sigma_param()))
#'
#' expect_false(is_alpha_param(NA))
#' expect_false(is_alpha_param(NULL))
#' expect_false(is_alpha_param("nonsense"))
#' expect_false(is_alpha_param(create_jc69_site_model()))
#' expect_false(is_alpha_param(create_strict_clock_model()))
#' expect_false(is_alpha_param(create_yule_tree_prior()))
#' expect_false(is_alpha_param(create_mcmc()))
#' @export
is_alpha_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "alpha"
}

#' Determine if the object is a valid
#' beta parameter
#' @param x an object, to be determined if it is a valid
#'   beta parameter
#' @return TRUE if x is a valid beta parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_false(is_beta_param(create_alpha_param()))
#' expect_true(is_beta_param(create_beta_param()))
#' expect_false(is_beta_param(create_clock_rate_param()))
#' expect_false(is_beta_param(create_kappa_1_param()))
#' expect_false(is_beta_param(create_kappa_2_param()))
#' expect_false(is_beta_param(create_lambda_param()))
#' expect_false(is_beta_param(create_m_param()))
#' expect_false(is_beta_param(create_mean_param()))
#' expect_false(is_beta_param(create_mu_param()))
#' expect_false(is_beta_param(create_rate_ac_param()))
#' expect_false(is_beta_param(create_rate_ag_param()))
#' expect_false(is_beta_param(create_rate_at_param()))
#' expect_false(is_beta_param(create_rate_cg_param()))
#' expect_false(is_beta_param(create_rate_ct_param()))
#' expect_false(is_beta_param(create_rate_gt_param()))
#' expect_false(is_beta_param(create_s_param()))
#' expect_false(is_beta_param(create_scale_param()))
#' expect_false(is_beta_param(create_sigma_param()))
#'
#' expect_false(is_beta_param(NA))
#' expect_false(is_beta_param(NULL))
#' expect_false(is_beta_param("nonsense"))
#' expect_false(is_beta_param(create_jc69_site_model()))
#' expect_false(is_beta_param(create_strict_clock_model()))
#' expect_false(is_beta_param(create_yule_tree_prior()))
#' expect_false(is_beta_param(create_mcmc()))
#' @export
is_beta_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_clock_rate_param(create_alpha_param()))
#' expect_false(is_clock_rate_param(create_beta_param()))
#' expect_true(is_clock_rate_param(create_clock_rate_param()))
#' expect_false(is_clock_rate_param(create_kappa_1_param()))
#' expect_false(is_clock_rate_param(create_kappa_2_param()))
#' expect_false(is_clock_rate_param(create_lambda_param()))
#' expect_false(is_clock_rate_param(create_m_param()))
#' expect_false(is_clock_rate_param(create_mean_param()))
#' expect_false(is_clock_rate_param(create_mu_param()))
#' expect_false(is_clock_rate_param(create_rate_ac_param()))
#' expect_false(is_clock_rate_param(create_rate_ag_param()))
#' expect_false(is_clock_rate_param(create_rate_at_param()))
#' expect_false(is_clock_rate_param(create_rate_cg_param()))
#' expect_false(is_clock_rate_param(create_rate_ct_param()))
#' expect_false(is_clock_rate_param(create_rate_gt_param()))
#' expect_false(is_clock_rate_param(create_s_param()))
#' expect_false(is_clock_rate_param(create_scale_param()))
#' expect_false(is_clock_rate_param(create_sigma_param()))
#'
#' expect_false(is_clock_rate_param(NA))
#' expect_false(is_clock_rate_param(NULL))
#' expect_false(is_clock_rate_param("nonsense"))
#' expect_false(is_clock_rate_param(create_jc69_site_model()))
#' expect_false(is_clock_rate_param(create_strict_clock_model()))
#' expect_false(is_clock_rate_param(create_yule_tree_prior()))
#' expect_false(is_clock_rate_param(create_mcmc()))
#' @export
is_clock_rate_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "clock_rate"
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
#' library(testthat)
#'
#' expect_false(is_kappa_1_param(create_alpha_param()))
#' expect_false(is_kappa_1_param(create_beta_param()))
#' expect_false(is_kappa_1_param(create_clock_rate_param()))
#' expect_true(is_kappa_1_param(create_kappa_1_param()))
#' expect_false(is_kappa_1_param(create_kappa_2_param()))
#' expect_false(is_kappa_1_param(create_lambda_param()))
#' expect_false(is_kappa_1_param(create_m_param()))
#' expect_false(is_kappa_1_param(create_mean_param()))
#' expect_false(is_kappa_1_param(create_mu_param()))
#' expect_false(is_kappa_1_param(create_rate_ac_param()))
#' expect_false(is_kappa_1_param(create_rate_ag_param()))
#' expect_false(is_kappa_1_param(create_rate_at_param()))
#' expect_false(is_kappa_1_param(create_rate_cg_param()))
#' expect_false(is_kappa_1_param(create_rate_ct_param()))
#' expect_false(is_kappa_1_param(create_rate_gt_param()))
#' expect_false(is_kappa_1_param(create_s_param()))
#' expect_false(is_kappa_1_param(create_scale_param()))
#' expect_false(is_kappa_1_param(create_sigma_param()))
#'
#' expect_false(is_kappa_1_param(NA))
#' expect_false(is_kappa_1_param(NULL))
#' expect_false(is_kappa_1_param("nonsense"))
#' expect_false(is_kappa_1_param(create_jc69_site_model()))
#' expect_false(is_kappa_1_param(create_strict_clock_model()))
#' expect_false(is_kappa_1_param(create_yule_tree_prior()))
#' expect_false(is_kappa_1_param(create_mcmc()))
#' @export
is_kappa_1_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_kappa_2_param(create_alpha_param()))
#' expect_false(is_kappa_2_param(create_beta_param()))
#' expect_false(is_kappa_2_param(create_clock_rate_param()))
#' expect_false(is_kappa_2_param(create_kappa_1_param()))
#' expect_true(is_kappa_2_param(create_kappa_2_param()))
#' expect_false(is_kappa_2_param(create_lambda_param()))
#' expect_false(is_kappa_2_param(create_m_param()))
#' expect_false(is_kappa_2_param(create_mean_param()))
#' expect_false(is_kappa_2_param(create_mu_param()))
#' expect_false(is_kappa_2_param(create_rate_ac_param()))
#' expect_false(is_kappa_2_param(create_rate_ag_param()))
#' expect_false(is_kappa_2_param(create_rate_at_param()))
#' expect_false(is_kappa_2_param(create_rate_cg_param()))
#' expect_false(is_kappa_2_param(create_rate_ct_param()))
#' expect_false(is_kappa_2_param(create_rate_gt_param()))
#' expect_false(is_kappa_2_param(create_s_param()))
#' expect_false(is_kappa_2_param(create_scale_param()))
#' expect_false(is_kappa_2_param(create_sigma_param()))
#'
#' expect_false(is_kappa_2_param(NA))
#' expect_false(is_kappa_2_param(NULL))
#' expect_false(is_kappa_2_param("nonsense"))
#' expect_false(is_kappa_2_param(create_jc69_site_model()))
#' expect_false(is_kappa_2_param(create_strict_clock_model()))
#' expect_false(is_kappa_2_param(create_yule_tree_prior()))
#' expect_false(is_kappa_2_param(create_mcmc()))
#' @export
is_kappa_2_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_lambda_param(create_alpha_param()))
#' expect_false(is_lambda_param(create_beta_param()))
#' expect_false(is_lambda_param(create_clock_rate_param()))
#' expect_false(is_lambda_param(create_kappa_1_param()))
#' expect_false(is_lambda_param(create_kappa_2_param()))
#' expect_true(is_lambda_param(create_lambda_param()))
#' expect_false(is_lambda_param(create_m_param()))
#' expect_false(is_lambda_param(create_mean_param()))
#' expect_false(is_lambda_param(create_mu_param()))
#' expect_false(is_lambda_param(create_rate_ac_param()))
#' expect_false(is_lambda_param(create_rate_ag_param()))
#' expect_false(is_lambda_param(create_rate_at_param()))
#' expect_false(is_lambda_param(create_rate_cg_param()))
#' expect_false(is_lambda_param(create_rate_ct_param()))
#' expect_false(is_lambda_param(create_rate_gt_param()))
#' expect_false(is_lambda_param(create_s_param()))
#' expect_false(is_lambda_param(create_scale_param()))
#' expect_false(is_lambda_param(create_sigma_param()))
#'
#' expect_false(is_lambda_param(NA))
#' expect_false(is_lambda_param(NULL))
#' expect_false(is_lambda_param("nonsense"))
#' expect_false(is_lambda_param(create_jc69_site_model()))
#' expect_false(is_lambda_param(create_strict_clock_model()))
#' expect_false(is_lambda_param(create_yule_tree_prior()))
#' expect_false(is_lambda_param(create_mcmc()))
#' @export
is_lambda_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "lambda"
}

#' Determine if the object is a valid
#' m parameter
#' @param x an object, to be determined if it is a valid
#'   m parameter
#' @return TRUE if x is a valid m parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_false(is_m_param(create_alpha_param()))
#' expect_false(is_m_param(create_beta_param()))
#' expect_false(is_m_param(create_clock_rate_param()))
#' expect_false(is_m_param(create_kappa_1_param()))
#' expect_false(is_m_param(create_kappa_2_param()))
#' expect_false(is_m_param(create_lambda_param()))
#' expect_true(is_m_param(create_m_param()))
#' expect_false(is_m_param(create_mean_param()))
#' expect_false(is_m_param(create_mu_param()))
#' expect_false(is_m_param(create_rate_ac_param()))
#' expect_false(is_m_param(create_rate_ag_param()))
#' expect_false(is_m_param(create_rate_at_param()))
#' expect_false(is_m_param(create_rate_cg_param()))
#' expect_false(is_m_param(create_rate_ct_param()))
#' expect_false(is_m_param(create_rate_gt_param()))
#' expect_false(is_m_param(create_s_param()))
#' expect_false(is_m_param(create_scale_param()))
#' expect_false(is_m_param(create_sigma_param()))
#'
#' expect_false(is_m_param(NA))
#' expect_false(is_m_param(NULL))
#' expect_false(is_m_param("nonsense"))
#' expect_false(is_m_param(create_jc69_site_model()))
#' expect_false(is_m_param(create_strict_clock_model()))
#' expect_false(is_m_param(create_yule_tree_prior()))
#' expect_false(is_m_param(create_mcmc()))
#' @export
is_m_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "m"
}

#' Determine if the object is a valid mean parameter
#' @param x an object, to be determined if it is a valid mean parameter,
#'   as created by \code{\link{create_mean_param}})
#' @return TRUE if x is a valid mean parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_false(is_mean_param(create_alpha_param()))
#' expect_false(is_mean_param(create_beta_param()))
#' expect_false(is_mean_param(create_clock_rate_param()))
#' expect_false(is_mean_param(create_kappa_1_param()))
#' expect_false(is_mean_param(create_kappa_2_param()))
#' expect_false(is_mean_param(create_lambda_param()))
#' expect_false(is_mean_param(create_m_param()))
#' expect_true(is_mean_param(create_mean_param()))
#' expect_false(is_mean_param(create_mu_param()))
#' expect_false(is_mean_param(create_rate_ac_param()))
#' expect_false(is_mean_param(create_rate_ag_param()))
#' expect_false(is_mean_param(create_rate_at_param()))
#' expect_false(is_mean_param(create_rate_cg_param()))
#' expect_false(is_mean_param(create_rate_ct_param()))
#' expect_false(is_mean_param(create_rate_gt_param()))
#' expect_false(is_mean_param(create_s_param()))
#' expect_false(is_mean_param(create_scale_param()))
#' expect_false(is_mean_param(create_sigma_param()))
#'
#' expect_false(is_mean_param(NA))
#' expect_false(is_mean_param(NULL))
#' expect_false(is_mean_param("nonsense"))
#' expect_false(is_mean_param(create_jc69_site_model()))
#' expect_false(is_mean_param(create_strict_clock_model()))
#' expect_false(is_mean_param(create_yule_tree_prior()))
#' expect_false(is_mean_param(create_mcmc()))
#' @export
is_mean_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_mu_param(create_alpha_param()))
#' expect_false(is_mu_param(create_beta_param()))
#' expect_false(is_mu_param(create_clock_rate_param()))
#' expect_false(is_mu_param(create_kappa_1_param()))
#' expect_false(is_mu_param(create_kappa_2_param()))
#' expect_false(is_mu_param(create_lambda_param()))
#' expect_false(is_mu_param(create_m_param()))
#' expect_false(is_mu_param(create_mean_param()))
#' expect_true(is_mu_param(create_mu_param()))
#' expect_false(is_mu_param(create_rate_ac_param()))
#' expect_false(is_mu_param(create_rate_ag_param()))
#' expect_false(is_mu_param(create_rate_at_param()))
#' expect_false(is_mu_param(create_rate_cg_param()))
#' expect_false(is_mu_param(create_rate_ct_param()))
#' expect_false(is_mu_param(create_rate_gt_param()))
#' expect_false(is_mu_param(create_s_param()))
#' expect_false(is_mu_param(create_scale_param()))
#' expect_false(is_mu_param(create_sigma_param()))
#'
#' expect_false(is_mu_param(NA))
#' expect_false(is_mu_param(NULL))
#' expect_false(is_mu_param("nonsense"))
#' expect_false(is_mu_param(create_jc69_site_model()))
#' expect_false(is_mu_param(create_strict_clock_model()))
#' expect_false(is_mu_param(create_yule_tree_prior()))
#' expect_false(is_mu_param(create_mcmc()))
#' @export
is_mu_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_rate_ac_param(create_alpha_param()))
#' expect_false(is_rate_ac_param(create_beta_param()))
#' expect_false(is_rate_ac_param(create_clock_rate_param()))
#' expect_false(is_rate_ac_param(create_kappa_1_param()))
#' expect_false(is_rate_ac_param(create_kappa_2_param()))
#' expect_false(is_rate_ac_param(create_lambda_param()))
#' expect_false(is_rate_ac_param(create_m_param()))
#' expect_false(is_rate_ac_param(create_mean_param()))
#' expect_false(is_rate_ac_param(create_mu_param()))
#' expect_true(is_rate_ac_param(create_rate_ac_param()))
#' expect_false(is_rate_ac_param(create_rate_ag_param()))
#' expect_false(is_rate_ac_param(create_rate_at_param()))
#' expect_false(is_rate_ac_param(create_rate_cg_param()))
#' expect_false(is_rate_ac_param(create_rate_ct_param()))
#' expect_false(is_rate_ac_param(create_rate_gt_param()))
#' expect_false(is_rate_ac_param(create_s_param()))
#' expect_false(is_rate_ac_param(create_scale_param()))
#' expect_false(is_rate_ac_param(create_sigma_param()))
#'
#' expect_false(is_rate_ac_param(NA))
#' expect_false(is_rate_ac_param(NULL))
#' expect_false(is_rate_ac_param("nonsense"))
#' expect_false(is_rate_ac_param(create_jc69_site_model()))
#' expect_false(is_rate_ac_param(create_strict_clock_model()))
#' expect_false(is_rate_ac_param(create_yule_tree_prior()))
#' expect_false(is_rate_ac_param(create_mcmc()))
#' @export
is_rate_ac_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_rate_ag_param(create_alpha_param()))
#' expect_false(is_rate_ag_param(create_beta_param()))
#' expect_false(is_rate_ag_param(create_clock_rate_param()))
#' expect_false(is_rate_ag_param(create_kappa_1_param()))
#' expect_false(is_rate_ag_param(create_kappa_2_param()))
#' expect_false(is_rate_ag_param(create_lambda_param()))
#' expect_false(is_rate_ag_param(create_m_param()))
#' expect_false(is_rate_ag_param(create_mean_param()))
#' expect_false(is_rate_ag_param(create_mu_param()))
#' expect_false(is_rate_ag_param(create_rate_ac_param()))
#' expect_true(is_rate_ag_param(create_rate_ag_param()))
#' expect_false(is_rate_ag_param(create_rate_at_param()))
#' expect_false(is_rate_ag_param(create_rate_cg_param()))
#' expect_false(is_rate_ag_param(create_rate_ct_param()))
#' expect_false(is_rate_ag_param(create_rate_gt_param()))
#' expect_false(is_rate_ag_param(create_s_param()))
#' expect_false(is_rate_ag_param(create_scale_param()))
#' expect_false(is_rate_ag_param(create_sigma_param()))
#'
#' expect_false(is_rate_ag_param(NA))
#' expect_false(is_rate_ag_param(NULL))
#' expect_false(is_rate_ag_param("nonsense"))
#' expect_false(is_rate_ag_param(create_jc69_site_model()))
#' expect_false(is_rate_ag_param(create_strict_clock_model()))
#' expect_false(is_rate_ag_param(create_yule_tree_prior()))
#' expect_false(is_rate_ag_param(create_mcmc()))
#' @export
is_rate_ag_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_rate_at_param(create_alpha_param()))
#' expect_false(is_rate_at_param(create_beta_param()))
#' expect_false(is_rate_at_param(create_clock_rate_param()))
#' expect_false(is_rate_at_param(create_kappa_1_param()))
#' expect_false(is_rate_at_param(create_kappa_2_param()))
#' expect_false(is_rate_at_param(create_lambda_param()))
#' expect_false(is_rate_at_param(create_m_param()))
#' expect_false(is_rate_at_param(create_mean_param()))
#' expect_false(is_rate_at_param(create_mu_param()))
#' expect_false(is_rate_at_param(create_rate_ac_param()))
#' expect_false(is_rate_at_param(create_rate_ag_param()))
#' expect_true(is_rate_at_param(create_rate_at_param()))
#' expect_false(is_rate_at_param(create_rate_cg_param()))
#' expect_false(is_rate_at_param(create_rate_ct_param()))
#' expect_false(is_rate_at_param(create_rate_gt_param()))
#' expect_false(is_rate_at_param(create_s_param()))
#' expect_false(is_rate_at_param(create_scale_param()))
#' expect_false(is_rate_at_param(create_sigma_param()))
#'
#' expect_false(is_rate_at_param(NA))
#' expect_false(is_rate_at_param(NULL))
#' expect_false(is_rate_at_param("nonsense"))
#' expect_false(is_rate_at_param(create_jc69_site_model()))
#' expect_false(is_rate_at_param(create_strict_clock_model()))
#' expect_false(is_rate_at_param(create_yule_tree_prior()))
#' expect_false(is_rate_at_param(create_mcmc()))
#' @export
is_rate_at_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_rate_cg_param(create_alpha_param()))
#' expect_false(is_rate_cg_param(create_beta_param()))
#' expect_false(is_rate_cg_param(create_clock_rate_param()))
#' expect_false(is_rate_cg_param(create_kappa_1_param()))
#' expect_false(is_rate_cg_param(create_kappa_2_param()))
#' expect_false(is_rate_cg_param(create_lambda_param()))
#' expect_false(is_rate_cg_param(create_m_param()))
#' expect_false(is_rate_cg_param(create_mean_param()))
#' expect_false(is_rate_cg_param(create_mu_param()))
#' expect_false(is_rate_cg_param(create_rate_ac_param()))
#' expect_false(is_rate_cg_param(create_rate_ag_param()))
#' expect_false(is_rate_cg_param(create_rate_at_param()))
#' expect_true(is_rate_cg_param(create_rate_cg_param()))
#' expect_false(is_rate_cg_param(create_rate_ct_param()))
#' expect_false(is_rate_cg_param(create_rate_gt_param()))
#' expect_false(is_rate_cg_param(create_s_param()))
#' expect_false(is_rate_cg_param(create_scale_param()))
#' expect_false(is_rate_cg_param(create_sigma_param()))
#'
#' expect_false(is_rate_cg_param(NA))
#' expect_false(is_rate_cg_param(NULL))
#' expect_false(is_rate_cg_param("nonsense"))
#' expect_false(is_rate_cg_param(create_jc69_site_model()))
#' expect_false(is_rate_cg_param(create_strict_clock_model()))
#' expect_false(is_rate_cg_param(create_yule_tree_prior()))
#' expect_false(is_rate_cg_param(create_mcmc()))
#' @export
is_rate_cg_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_rate_ct_param(create_alpha_param()))
#' expect_false(is_rate_ct_param(create_beta_param()))
#' expect_false(is_rate_ct_param(create_clock_rate_param()))
#' expect_false(is_rate_ct_param(create_kappa_1_param()))
#' expect_false(is_rate_ct_param(create_kappa_2_param()))
#' expect_false(is_rate_ct_param(create_lambda_param()))
#' expect_false(is_rate_ct_param(create_m_param()))
#' expect_false(is_rate_ct_param(create_mean_param()))
#' expect_false(is_rate_ct_param(create_mu_param()))
#' expect_false(is_rate_ct_param(create_rate_ac_param()))
#' expect_false(is_rate_ct_param(create_rate_ag_param()))
#' expect_false(is_rate_ct_param(create_rate_at_param()))
#' expect_false(is_rate_ct_param(create_rate_cg_param()))
#' expect_true(is_rate_ct_param(create_rate_ct_param()))
#' expect_false(is_rate_ct_param(create_rate_gt_param()))
#' expect_false(is_rate_ct_param(create_s_param()))
#' expect_false(is_rate_ct_param(create_scale_param()))
#' expect_false(is_rate_ct_param(create_sigma_param()))
#'
#' expect_false(is_rate_ct_param(NA))
#' expect_false(is_rate_ct_param(NULL))
#' expect_false(is_rate_ct_param("nonsense"))
#' expect_false(is_rate_ct_param(create_jc69_site_model()))
#' expect_false(is_rate_ct_param(create_strict_clock_model()))
#' expect_false(is_rate_ct_param(create_yule_tree_prior()))
#' expect_false(is_rate_ct_param(create_mcmc()))
#' @export
is_rate_ct_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_rate_gt_param(create_alpha_param()))
#' expect_false(is_rate_gt_param(create_beta_param()))
#' expect_false(is_rate_gt_param(create_clock_rate_param()))
#' expect_false(is_rate_gt_param(create_kappa_1_param()))
#' expect_false(is_rate_gt_param(create_kappa_2_param()))
#' expect_false(is_rate_gt_param(create_lambda_param()))
#' expect_false(is_rate_gt_param(create_m_param()))
#' expect_false(is_rate_gt_param(create_mean_param()))
#' expect_false(is_rate_gt_param(create_mu_param()))
#' expect_false(is_rate_gt_param(create_rate_ac_param()))
#' expect_false(is_rate_gt_param(create_rate_ag_param()))
#' expect_false(is_rate_gt_param(create_rate_at_param()))
#' expect_false(is_rate_gt_param(create_rate_cg_param()))
#' expect_false(is_rate_gt_param(create_rate_ct_param()))
#' expect_true(is_rate_gt_param(create_rate_gt_param()))
#' expect_false(is_rate_gt_param(create_s_param()))
#' expect_false(is_rate_gt_param(create_scale_param()))
#' expect_false(is_rate_gt_param(create_sigma_param()))
#'
#' expect_false(is_rate_gt_param(NA))
#' expect_false(is_rate_gt_param(NULL))
#' expect_false(is_rate_gt_param("nonsense"))
#' expect_false(is_rate_gt_param(create_jc69_site_model()))
#' expect_false(is_rate_gt_param(create_strict_clock_model()))
#' expect_false(is_rate_gt_param(create_yule_tree_prior()))
#' expect_false(is_rate_gt_param(create_mcmc()))
#' @export
is_rate_gt_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_s_param(create_alpha_param()))
#' expect_false(is_s_param(create_beta_param()))
#' expect_false(is_s_param(create_clock_rate_param()))
#' expect_false(is_s_param(create_kappa_1_param()))
#' expect_false(is_s_param(create_kappa_2_param()))
#' expect_false(is_s_param(create_lambda_param()))
#' expect_false(is_s_param(create_m_param()))
#' expect_false(is_s_param(create_mean_param()))
#' expect_false(is_s_param(create_mu_param()))
#' expect_false(is_s_param(create_rate_ac_param()))
#' expect_false(is_s_param(create_rate_ag_param()))
#' expect_false(is_s_param(create_rate_at_param()))
#' expect_false(is_s_param(create_rate_cg_param()))
#' expect_false(is_s_param(create_rate_ct_param()))
#' expect_false(is_s_param(create_rate_gt_param()))
#' expect_true(is_s_param(create_s_param()))
#' expect_false(is_s_param(create_scale_param()))
#' expect_false(is_s_param(create_sigma_param()))
#'
#' expect_false(is_s_param(NA))
#' expect_false(is_s_param(NULL))
#' expect_false(is_s_param("nonsense"))
#' expect_false(is_s_param(create_jc69_site_model()))
#' expect_false(is_s_param(create_strict_clock_model()))
#' expect_false(is_s_param(create_yule_tree_prior()))
#' expect_false(is_s_param(create_mcmc()))
#' @export
is_s_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_scale_param(create_alpha_param()))
#' expect_false(is_scale_param(create_beta_param()))
#' expect_false(is_scale_param(create_clock_rate_param()))
#' expect_false(is_scale_param(create_kappa_1_param()))
#' expect_false(is_scale_param(create_kappa_2_param()))
#' expect_false(is_scale_param(create_lambda_param()))
#' expect_false(is_scale_param(create_m_param()))
#' expect_false(is_scale_param(create_mean_param()))
#' expect_false(is_scale_param(create_mu_param()))
#' expect_false(is_scale_param(create_rate_ac_param()))
#' expect_false(is_scale_param(create_rate_ag_param()))
#' expect_false(is_scale_param(create_rate_at_param()))
#' expect_false(is_scale_param(create_rate_cg_param()))
#' expect_false(is_scale_param(create_rate_ct_param()))
#' expect_false(is_scale_param(create_rate_gt_param()))
#' expect_false(is_scale_param(create_s_param()))
#' expect_true(is_scale_param(create_scale_param()))
#' expect_false(is_scale_param(create_sigma_param()))
#'
#' expect_false(is_scale_param(NA))
#' expect_false(is_scale_param(NULL))
#' expect_false(is_scale_param("nonsense"))
#' expect_false(is_scale_param(create_jc69_site_model()))
#' expect_false(is_scale_param(create_strict_clock_model()))
#' expect_false(is_scale_param(create_yule_tree_prior()))
#' expect_false(is_scale_param(create_mcmc()))
#' @export
is_scale_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
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
#' library(testthat)
#'
#' expect_false(is_sigma_param(create_alpha_param()))
#' expect_false(is_sigma_param(create_beta_param()))
#' expect_false(is_sigma_param(create_clock_rate_param()))
#' expect_false(is_sigma_param(create_kappa_1_param()))
#' expect_false(is_sigma_param(create_kappa_2_param()))
#' expect_false(is_sigma_param(create_lambda_param()))
#' expect_false(is_sigma_param(create_m_param()))
#' expect_false(is_sigma_param(create_mean_param()))
#' expect_false(is_sigma_param(create_mu_param()))
#' expect_false(is_sigma_param(create_rate_ac_param()))
#' expect_false(is_sigma_param(create_rate_ag_param()))
#' expect_false(is_sigma_param(create_rate_at_param()))
#' expect_false(is_sigma_param(create_rate_cg_param()))
#' expect_false(is_sigma_param(create_rate_ct_param()))
#' expect_false(is_sigma_param(create_rate_gt_param()))
#' expect_false(is_sigma_param(create_s_param()))
#' expect_false(is_sigma_param(create_scale_param()))
#' expect_true(is_sigma_param(create_sigma_param()))
#'
#' expect_false(is_sigma_param(NA))
#' expect_false(is_sigma_param(NULL))
#' expect_false(is_sigma_param("nonsense"))
#' expect_false(is_sigma_param(create_jc69_site_model()))
#' expect_false(is_sigma_param(create_strict_clock_model()))
#' expect_false(is_sigma_param(create_yule_tree_prior()))
#' expect_false(is_sigma_param(create_mcmc()))
#' @export
is_sigma_param <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  x$name == "sigma"
}
