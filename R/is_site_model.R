#' Determine if the object is a valid site_model
#' @param x an object, to be determined if it is a site_model
#' @return TRUE if the site_model is a valid site_model, FALSE otherwise
#' @seealso  A site model can be created using \code{\link{create_site_model}}
#' @examples
#' library(testthat)
#'
#' # site models
#' expect_true(is_site_model(create_gtr_site_model()))
#' expect_true(is_site_model(create_hky_site_model()))
#' expect_true(is_site_model(create_jc69_site_model()))
#' expect_true(is_site_model(create_tn93_site_model()))
#'
#' # other models
#' expect_false(is_site_model(NA))
#' expect_false(is_site_model(NULL))
#' expect_false(is_site_model("nonsense"))
#' expect_false(is_site_model(create_strict_clock_model()))
#' expect_false(is_site_model(create_bd_tree_prior()))
#' expect_false(is_site_model(create_mcmc()))
#' @export
is_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!beautier::is_site_model_name(x$name)) return(FALSE)
  if (!"id" %in% names(x)) return(FALSE)
  if (!"gamma_site_model" %in% names(x)) return(FALSE)
  if (!beautier::is_gamma_site_model(x$gamma_site_model)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid GTR site model,
#' as created by \code{\link{create_gtr_site_model}}
#' @param x an object, to be determined if it is a valid GTR site model
#' @return TRUE if x is a valid GTR site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' # site models
#' expect_true(is_gtr_site_model(create_gtr_site_model()))
#' expect_false(is_gtr_site_model(create_hky_site_model()))
#' expect_false(is_gtr_site_model(create_jc69_site_model()))
#' expect_false(is_gtr_site_model(create_tn93_site_model()))
#'
#' # other models
#' expect_false(is_gtr_site_model(NA))
#' expect_false(is_gtr_site_model(NULL))
#' expect_false(is_gtr_site_model("nonsense"))
#' expect_false(is_gtr_site_model(create_strict_clock_model()))
#' expect_false(is_gtr_site_model(create_bd_tree_prior()))
#' expect_false(is_gtr_site_model(create_mcmc()))
#' @export
is_gtr_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE) # nolint beautier function
  if (x$name != "GTR") return(FALSE)

  expected_names <- c("rate_ac_prior_distr", "rate_ag_prior_distr",
    "rate_at_prior_distr", "rate_cg_prior_distr", "rate_gt_prior_distr",
    "rate_ac_param", "rate_ag_param", "rate_at_param", "rate_cg_param",
    "rate_ct_param", "rate_gt_param", "freq_equilibrium")
  for (expected_name in expected_names) {
    if (!expected_name %in% names(x)) return(FALSE)
  }

  expected_distrs <- list(x$rate_ac_prior_distr, x$rate_ag_prior_distr,
    x$rate_at_prior_distr, x$rate_cg_prior_distr, x$rate_gt_prior_distr)
  for (expected_distr in expected_distrs) {
    if (!beautier::is_distr(expected_distr)) return(FALSE)
  }

  expected_params <- list(x$rate_ac_param, x$rate_ag_param, x$rate_at_param,
    x$rate_cg_param, x$rate_ct_param, x$rate_gt_param)
  for (expected_param in expected_params) {
    if (!beautier::is_param(expected_param)) return(FALSE)
  }

  if (!beautier::is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid HKY site model,
#' as created by \code{\link{create_hky_site_model}}
#' @param x an object, to be determined if it is a valid HKY site model
#' @return TRUE if x is a valid HKY site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' # site models
#' expect_true(is_hky_site_model(create_hky_site_model()))
#' expect_false(is_hky_site_model(create_gtr_site_model()))
#' expect_false(is_hky_site_model(create_jc69_site_model()))
#' expect_false(is_hky_site_model(create_tn93_site_model()))
#'
#' # other models
#' expect_false(is_hky_site_model(NA))
#' expect_false(is_hky_site_model(NULL))
#' expect_false(is_hky_site_model("nonsense"))
#' expect_false(is_hky_site_model(create_strict_clock_model()))
#' expect_false(is_hky_site_model(create_bd_tree_prior()))
#' expect_false(is_hky_site_model(create_mcmc()))
#' @export
is_hky_site_model <- function(
  x
) {
  if (!beautier::is_site_model(x)) return(FALSE)
  if (x$name != "HKY") return(FALSE)
  if (!"kappa" %in% names(x)) return(FALSE)
  if (!"kappa_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$kappa_prior_distr)) return(FALSE)
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!beautier::is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid JC69 site model
#' @param x an object, to be determined if it is a valid JC69 site model
#' @return TRUE if x is a valid JC69 site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' # site models
#' expect_false(is_jc69_site_model(create_gtr_site_model()))
#' expect_false(is_jc69_site_model(create_hky_site_model()))
#' expect_true(is_jc69_site_model(create_jc69_site_model()))
#' expect_false(is_jc69_site_model(create_tn93_site_model()))
#'
#' # other models
#' expect_false(is_jc69_site_model(NA))
#' expect_false(is_jc69_site_model(NULL))
#' expect_false(is_jc69_site_model("nonsense"))
#' expect_false(is_jc69_site_model(create_strict_clock_model()))
#' expect_false(is_jc69_site_model(create_bd_tree_prior()))
#' expect_false(is_jc69_site_model(create_mcmc()))
#' @export
is_jc69_site_model <- function(
  x
) {
  if (!beautier::is_site_model(x)) return(FALSE)
  if (x$name != "JC69") return(FALSE)
  TRUE
}

#' Determine if the object is a valid TN93 site model,
#' @param x an object, to be determined if it is a valid TN93 site model,
#'   as created by \code{\link{create_tn93_site_model}}
#' @return TRUE if x is a valid TN93 site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' # site models
#' expect_false(is_tn93_site_model(create_gtr_site_model()))
#' expect_false(is_tn93_site_model(create_hky_site_model()))
#' expect_false(is_tn93_site_model(create_jc69_site_model()))
#' expect_true(is_tn93_site_model(create_tn93_site_model()))
#'
#' # other models
#' expect_false(is_tn93_site_model(NA))
#' expect_false(is_tn93_site_model(NULL))
#' expect_false(is_tn93_site_model("nonsense"))
#' expect_false(is_tn93_site_model(create_strict_clock_model()))
#' expect_false(is_tn93_site_model(create_bd_tree_prior()))
#' expect_false(is_tn93_site_model(create_mcmc()))
#' @export
is_tn93_site_model <- function(
  x
) {
  if (!beautier::is_site_model(x)) return(FALSE)
  if (!"kappa_1_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$kappa_1_prior_distr)) return(FALSE)
  if (!"kappa_2_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$kappa_2_prior_distr)) return(FALSE)
  if (!"kappa_1_param" %in% names(x)) return(FALSE)
  if (!beautier::is_param(x$kappa_1_param)) return(FALSE)
  if (!"kappa_2_param" %in% names(x)) return(FALSE)
  if (!beautier::is_param(x$kappa_2_param)) return(FALSE)
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!beautier::is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE
}
