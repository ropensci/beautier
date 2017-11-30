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
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "GTR") return(FALSE)
  if (!"gamma_site_model" %in% names(x)) return(FALSE)
  if (!"rate_ac_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$rate_ac_prior_distr)) return(FALSE)
  if (!"rate_ag_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$rate_ag_prior_distr)) return(FALSE)
  if (!"rate_at_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$rate_at_prior_distr)) return(FALSE)
  if (!"rate_cg_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$rate_cg_prior_distr)) return(FALSE)
  if (!"rate_gt_prior_distr" %in% names(x)) return(FALSE)
  if (!beautier::is_distr(x$rate_gt_prior_distr)) return(FALSE)
  if (!"rate_ac_param" %in% names(x)) return(FALSE)
  if (!beautier::is_param(x$rate_ac_param)) return(FALSE)
  if (!"rate_ag_param" %in% names(x)) return(FALSE)
  if (!beautier::is_param(x$rate_ag_param)) return(FALSE)
  if (!"rate_at_param" %in% names(x)) return(FALSE)
  if (!beautier::is_param(x$rate_at_param)) return(FALSE)
  if (!"rate_cg_param" %in% names(x)) return(FALSE)
  if (!beautier::is_param(x$rate_cg_param)) return(FALSE)
  if (!"rate_gt_param" %in% names(x)) return(FALSE)
  if (!beautier::is_param(x$rate_gt_param)) return(FALSE)
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!beautier::is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE
}
