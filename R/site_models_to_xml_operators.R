#' Write the XML \code{operators} section from the site models.
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
site_models_to_xml_operators <- function(
  site_models
) {
  check_true(are_site_models(site_models))

  text <- NULL
  for (site_model in site_models) {
    text <- c(text, site_model_to_xml_operators(site_model))
  }
  text
}
