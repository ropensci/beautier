#' Converts a site model to XML,
#'   used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
jc69_site_model_to_xml_state <- function(
  site_model,
  beauti_options = create_beauti_options()
) {
  testit::assert(is_site_model(site_model))
  check_beauti_options(beauti_options)
  id <- site_model$id
  testit::assert(is_id(id))
  testthat::expect_true(is_jc69_site_model(site_model))
  text <- NULL
  text <- c(
    text,
    gamma_site_model_to_xml_state(site_model$gamma_site_model, id)
  )
  text
}
