#' Converts a gamma site model to XML,
#'   used in the \code{state} section
#' @param id the site model's ID
#' @param gamma_site_model a gamma site model,
#'   as created by \code{\link{create_gamma_site_model}})
#' @return the gamma_site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
gamma_site_model_to_xml_state <- function(
  gamma_site_model,
  id
) {
  testit::assert(is_gamma_site_model(gamma_site_model)) # nolint beautier function
  testit::assert(is_id(id)) # nolint beautier function
  text <- NULL
  if (gamma_site_model$gamma_cat_count >= 2) {
    text <- c(
      text,
      paste0("<parameter id=\"gammaShape.s:", id, "\" ",
        "name=\"stateNode\">",
        gamma_site_model$gamma_shape,
        "</parameter>"
      )
    )
  }
  text
}
