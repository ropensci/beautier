#' Determine if the object is a valid TN93 site model,
#' @param x an object, to be determined if it is a valid TN93 site model,
#'   as created by \code{\link{create_tn93_site_model}}
#' @return TRUE if x is a valid TN93 site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'   tn93_site_model <- create_tn93_site_model()
#'   testit::assert(is_tn93_site_model(tn93_site_model))
#'
#'  create_beast2_input_file(
#'    input_fasta_filenames = get_input_fasta_filename(),
#'    "beast.xml",
#'    site_models = tn93_site_model
#'  )
#' @export
is_tn93_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "TN93") return(FALSE)
  if (!"kappa_1_prior_distr" %in% names(x)) return(FALSE)
  if (!"kappa_2_prior_distr" %in% names(x)) return(FALSE)
  TRUE

}
