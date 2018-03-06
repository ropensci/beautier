#' Converts a site model to XML,
#'   used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richel J.C. Bilderbeek
site_model_to_xml_state <- function(
  site_model
) {
  testit::assert(is_site_model(site_model))
  id <- site_model$id
  testit::assert(is_id(id))
  text <- NULL
  if (is_gtr_site_model(site_model)) {
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
      text <- c(text, parameter_to_xml(site_model$rate_ac_param)) # nolint internal function
    }
    if (site_model$rate_ag_param$estimate == TRUE) {
      text <- c(text, parameter_to_xml(site_model$rate_ag_param)) # nolint internal function
    }
    if (site_model$rate_at_param$estimate == TRUE) {
      text <- c(text, parameter_to_xml(site_model$rate_at_param)) # nolint internal function
    }
    if (site_model$rate_cg_param$estimate == TRUE) {
      text <- c(text, parameter_to_xml(site_model$rate_cg_param)) # nolint internal function
    }
    if (site_model$rate_ct_param$estimate == TRUE) {
      text <- c(text, parameter_to_xml(site_model$rate_ct_param)) # nolint internal function
    }
    if (site_model$rate_gt_param$estimate == TRUE) {
      text <- c(text, parameter_to_xml(site_model$rate_gt_param)) # nolint internal function
    }
  } else if (is_hky_site_model(site_model)) {
    site_model$kappa_param$id <- id
    text <- c(text, paste0("<parameter id=\"kappa.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">",
      site_model$kappa, "</parameter>"))
  } else if (is_tn93_site_model(site_model)) {
      site_model$kappa_1_param$id <- id
      site_model$kappa_2_param$id <- id
      text <- c(text, parameter_to_xml(site_model$kappa_1_param)) # nolint internal function
      text <- c(text, parameter_to_xml(site_model$kappa_2_param)) # nolint internal function
  }

  if (!is_jc69_site_model(site_model)) {
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
    gamma_site_model_to_xml_state(site_model$gamma_site_model, id)
  )
  text
}
