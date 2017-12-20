#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
site_models_to_xml_operators <- function(
  site_models
) {
  testit::assert(are_site_models(site_models))

  site_models <- get_unlinked_site_models(site_models) # nolint internal function

  text <- NULL
  for (site_model in site_models) {
    text <- c(text, site_model_to_xml_operators(site_model)) # nolint internal function
  }
  text
}
