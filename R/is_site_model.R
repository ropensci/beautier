#' Determine if the object is a valid site_model
#' @param x an object, to be determined if it is a site_model
#' @return TRUE if the site_model is a valid site_model, FALSE otherwise
#' @seealso  A site model can be created using \code{\link{create_site_model}}
#' @examples
#'   # site models
#'   testit::assert(is_site_model(create_gtr_site_model()))
#'   testit::assert(is_site_model(create_hky_site_model()))
#'   testit::assert(is_site_model(create_jc69_site_model()))
#'   testit::assert(is_site_model(create_tn93_site_model()))
#'
#'   # other models
#'   testit::assert(!is_site_model(create_strict_clock_model()))
#'   testit::assert(!is_site_model(create_bd_tree_prior()))
#'   testit::assert(!is_site_model(create_mcmc()))
#' @export
is_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!is_site_model_name(x$name)) return(FALSE) # nolint beautier function
  if (!"id" %in% names(x)) return(FALSE)
  if (!"gamma_site_model" %in% names(x)) return(FALSE)
  if (!is_gamma_site_model(x$gamma_site_model)) return(FALSE) # nolint beautier function
  TRUE
}

#' Determine if the object is a valid GTR site model,
#' as created by \code{\link{create_gtr_site_model}}
#' @param x an object, to be determined if it is a valid GTR site model
#' @return TRUE if x is a valid GTR site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   gtr_site_model <- create_gtr_site_model()
#'   testit::assert(beautier:::is_gtr_site_model(gtr_site_model))
#' @noRd
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
    if (!is_distr(expected_distr)) return(FALSE) # nolint beautier function
  }

  expected_params <- list(x$rate_ac_param, x$rate_ag_param, x$rate_at_param,
    x$rate_cg_param, x$rate_ct_param, x$rate_gt_param)
  for (expected_param in expected_params) {
    if (!is_param(expected_param)) return(FALSE) # nolint beautier function
  }

  if (!is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE) # nolint beautier function
  TRUE
}

#' Determine if the object is a valid HKY site model,
#' as created by \code{\link{create_hky_site_model}}
#' @param x an object, to be determined if it is a valid HKY site model
#' @return TRUE if x is a valid HKY site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   hky_site_model <- create_hky_site_model()
#'   testit::assert(beautier:::is_hky_site_model(hky_site_model))
#' @noRd
is_hky_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE) # nolint beautier function
  if (x$name != "HKY") return(FALSE)
  if (!"kappa" %in% names(x)) return(FALSE)
  if (!"kappa_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$kappa_prior_distr)) return(FALSE) # nolint beautier function
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE) # nolint beautier function
  TRUE
}

#' Determine if the object is a valid JC69 site model
#' @param x an object, to be determined if it is a valid JC69 site model
#' @return TRUE if x is a valid JC69 site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   jc69_site_model <- create_jc69_site_model()
#'   testit::assert(beautier:::is_jc69_site_model(jc69_site_model))
#' @noRd
is_jc69_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE) # nolint beautier function
  if (x$name != "JC69") return(FALSE)
  TRUE
}

#' Determine if the object is a valid TN93 site model,
#' @param x an object, to be determined if it is a valid TN93 site model,
#'   as created by \code{\link{create_tn93_site_model}}
#' @return TRUE if x is a valid TN93 site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  create_beast2_input_file(
#'    input_filename = get_fasta_filename(),
#'    "beast.xml",
#'    site_model = create_tn93_site_model()
#'  )
#' @noRd
is_tn93_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE) # nolint beautier function
  if (!"kappa_1_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$kappa_1_prior_distr)) return(FALSE) # nolint beautier function
  if (!"kappa_2_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$kappa_2_prior_distr)) return(FALSE) # nolint beautier function
  if (!"kappa_1_param" %in% names(x)) return(FALSE)
  if (!is_param(x$kappa_1_param)) return(FALSE) # nolint beautier function
  if (!"kappa_2_param" %in% names(x)) return(FALSE)
  if (!is_param(x$kappa_2_param)) return(FALSE) # nolint beautier function
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE) # nolint beautier function
  TRUE
}
