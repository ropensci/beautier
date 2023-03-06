#' Converts a site model to XML,
#'   used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
hky_site_model_to_xml_state <- function(
  site_model,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_site_model(site_model))
  beautier::check_beauti_options(beauti_options)
  id <- site_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  if (beautier::is_hky_site_model(site_model)) {
    testthat::expect_true("kappa_param" %in% names(site_model))
    site_model$kappa_param$id <- id
    text <- c(
      text,
      beautier::parameter_to_xml(
        site_model$kappa_param,
        beauti_options = beauti_options
      )
    )
  }

  if (!beautier::is_jc69_site_model(site_model)) {
    text <- c(
      text,
      paste0(
        "<parameter ",
        "id=\"freqParameter.s:", id, "\" dimension=\"4\" ",
        "lower=\"0.0\" ",
        "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
      )
    )
  }

  text <- c(
    text,
    beautier::gamma_site_model_to_xml_state(site_model$gamma_site_model, id)
  )
  text
}
