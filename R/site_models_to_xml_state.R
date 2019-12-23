#' Converts one or more clock models to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
site_models_to_xml_state <- function(
  site_models
) {
  testit::assert(beautier::are_site_models(site_models))

  text <- NULL
  for (site_model in site_models) {
    text <- c(text,
      beautier::site_model_to_xml_state(site_model)
    )
  }
  text
}
