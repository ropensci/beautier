#' Creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
gamma_site_models_to_xml_prior_distr <- function( # nolint internal function
  site_models
) {
  text <- NULL
  for (site_model in site_models) {
    text <- c(
      text,
      gamma_site_model_to_xml_prior_distr(site_model) # nolint
    )
  }
  text
}