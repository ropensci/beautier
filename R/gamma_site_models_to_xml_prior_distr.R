#' Deprecated function
#'
#' Internal function to
#' creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
gamma_site_models_to_xml_prior_distr <- function( # nolint indeed long function name
  site_models,
  beauti_options = create_beauti_options()
) {
  stop("Deprecated function, use 'gamma_site_model_to_xml_prior_distr' instead")
}
