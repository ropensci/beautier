#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
site_models_to_xml_operators <- function(
  site_models
) {
  testit::assert(are_site_models(site_models)) # nolint beautier function

  text <- NULL
  for (site_model in site_models) {
    text <- c(text, site_model_to_xml_operators(site_model)) # nolint beautier function
  }
  text
}
