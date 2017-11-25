#' Determine if the object is a valid GTR site model,
#' as created by \code{\link{create_gtr_site_model}}
#' @param x an object, to be determined if it is a valid GTR site model
#' @return TRUE if x is a valid GTR site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_gtr_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "GTR") return(FALSE)
  if (!"gamma_site_model" %in% names(x)) return(FALSE)
  if (!"gamma_0_alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$gamma_0_alpha)) return(FALSE)
  if (!"gamma_0_beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$gamma_0_beta)) return(FALSE)
  if (!"gamma_1_alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$gamma_1_alpha)) return(FALSE)
  if (!"gamma_1_beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$gamma_1_beta)) return(FALSE)
  if (!"gamma_2_alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$gamma_2_alpha)) return(FALSE)
  if (!"gamma_2_beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$gamma_2_beta)) return(FALSE)
  if (!"gamma_3_alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$gamma_3_alpha)) return(FALSE)
  if (!"gamma_3_beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$gamma_3_beta)) return(FALSE)
  if (!"gamma_5_alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$gamma_5_alpha)) return(FALSE)
  if (!"gamma_5_beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$gamma_5_beta)) return(FALSE)
  TRUE
}
