site_models_to_xml_tracelog <- function(
  site_models
) {
  text <- NULL
  for (site_model in site_models) {
    text <- c(text, site_model_to_xml_tracelog(site_model))
  }
  text
}

