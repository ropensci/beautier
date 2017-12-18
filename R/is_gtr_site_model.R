#' Determine if the object is a valid GTR site model,
#' as created by \code{\link{create_gtr_site_model}}
#' @param x an object, to be determined if it is a valid GTR site model
#' @return TRUE if x is a valid GTR site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'   gtr_site_model <- create_gtr_site_model()
#'   testit::assert(is_gtr_site_model(gtr_site_model))
#' @export
is_gtr_site_model <- function(
  x
) {
  if (!beautier::is_site_model(x)) return(FALSE)
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

  if (!beautier::is_param(x$rate_ac_param)) return(FALSE)
  if (!beautier::is_param(x$rate_ag_param)) return(FALSE)
  if (!beautier::is_param(x$rate_at_param)) return(FALSE)
  if (!beautier::is_param(x$rate_cg_param)) return(FALSE)
  if (!beautier::is_param(x$rate_ct_param)) return(FALSE)
  if (!beautier::is_param(x$rate_gt_param)) return(FALSE)
  if (!beautier::is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE
}
