#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
site_models_to_xml_operators <- function(
  site_models
) {
  testit::assert(beautier::are_site_models(site_models))

  text <- NULL
  for (site_model in site_models) {
    text <- c(text, site_model_to_xml_operators(site_model))
  }
  text
}
