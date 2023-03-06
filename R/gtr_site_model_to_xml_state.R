#' Converts a site model to XML,
#'   used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
gtr_site_model_to_xml_state <- function(
  site_model,
  beauti_options = create_beauti_options()
) {
  testit::assert(beautier::is_site_model(site_model))
  beautier::check_beauti_options(beauti_options)
  id <- site_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  testthat::expect_true(beautier::is_gtr_site_model(site_model))

  site_model$rate_ac_param$id <- id
  site_model$rate_ag_param$id <- id
  site_model$rate_at_param$id <- id
  site_model$rate_cg_param$id <- id
  site_model$rate_ct_param$id <- id
  site_model$rate_gt_param$id <- id
  testit::assert("estimate" %in% names(site_model$rate_ac_param))
  testit::assert("estimate" %in% names(site_model$rate_ag_param))
  testit::assert("estimate" %in% names(site_model$rate_at_param))
  testit::assert("estimate" %in% names(site_model$rate_cg_param))
  testit::assert("estimate" %in% names(site_model$rate_ct_param))
  testit::assert("estimate" %in% names(site_model$rate_gt_param))
  if (site_model$rate_ac_param$estimate == TRUE) {
    text <- c(
      text,
      beautier::parameter_to_xml(
        site_model$rate_ac_param,
        beauti_options = beauti_options
      )
    )
  }
  if (site_model$rate_ag_param$estimate == TRUE) {
    text <- c(
      text,
      beautier::parameter_to_xml(
        site_model$rate_ag_param,
        beauti_options = beauti_options
      )
    )
  }
  if (site_model$rate_at_param$estimate == TRUE) {
    text <- c(
      text,
      beautier::parameter_to_xml(
        site_model$rate_at_param,
        beauti_options = beauti_options
      )
    )
  }
  if (site_model$rate_cg_param$estimate == TRUE) {
    text <- c(
      text,
      beautier::parameter_to_xml(
        site_model$rate_cg_param,
        beauti_options = beauti_options
      )
    )
  }
  if (site_model$rate_ct_param$estimate == TRUE) {
    text <- c(
      text,
      beautier::parameter_to_xml(
        site_model$rate_ct_param,
        beauti_options = beauti_options
      )
    )
  }
  if (site_model$rate_gt_param$estimate == TRUE) {
    text <- c(
      text,
      beautier::parameter_to_xml(
        site_model$rate_gt_param,
        beauti_options = beauti_options
      )
    )
  }
  text <- c(
    text,
    paste0(
      "<parameter ",
      "id=\"freqParameter.s:", id, "\" dimension=\"4\" ",
      "lower=\"0.0\" ",
      "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
    )
  )
  text <- c(
    text,
    beautier::gamma_site_model_to_xml_state(site_model$gamma_site_model, id)
  )
  text
}
