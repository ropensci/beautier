#' Converts one or more clock models to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richel J.C. Bilderbeek
site_models_to_xml_state <- function(
  site_models
) {
  testit::assert(are_site_models(site_models))

  # Remove the clock models that share a same alignment
  site_models <- get_unlinked_site_models(site_models) # nolint internal function
  testit::assert(are_site_models(site_models))

  text <- NULL
  for (site_model in site_models) {
    text <- c(text,
      site_model_to_xml_state(site_model)
    )
  }
  text
}
