#' Determine if the object is a valid TN93 site model,
#' @param x an object, to be determined if it is a valid TN93 site model,
#'   as created by \code{\link{create_tn93_site_model}}
#' @return TRUE if x is a valid TN93 site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'  create_beast2_input_file(
#'    input_filenames = get_fasta_filename(),
#'    "beast.xml",
#'    site_models = create_tn93_site_model()
#'  )
is_tn93_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE)
  if (!"kappa_1_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$kappa_1_prior_distr)) return(FALSE)
  if (!"kappa_2_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$kappa_2_prior_distr)) return(FALSE)
  if (!"kappa_1_param" %in% names(x)) return(FALSE)
  if (!is_param(x$kappa_1_param)) return(FALSE)
  if (!"kappa_2_param" %in% names(x)) return(FALSE)
  if (!is_param(x$kappa_2_param)) return(FALSE)
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE

}
