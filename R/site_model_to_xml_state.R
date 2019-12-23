#' Converts a site model to XML,
#'   used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
site_model_to_xml_state <- function(
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  if (beautier::is_gtr_site_model(site_model)) {
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
      text <- c(text, beautier::parameter_to_xml(site_model$rate_ac_param))
    }
    if (site_model$rate_ag_param$estimate == TRUE) {
      text <- c(text, beautier::parameter_to_xml(site_model$rate_ag_param))
    }
    if (site_model$rate_at_param$estimate == TRUE) {
      text <- c(text, beautier::parameter_to_xml(site_model$rate_at_param))
    }
    if (site_model$rate_cg_param$estimate == TRUE) {
      text <- c(text, beautier::parameter_to_xml(site_model$rate_cg_param))
    }
    if (site_model$rate_ct_param$estimate == TRUE) {
      text <- c(text, beautier::parameter_to_xml(site_model$rate_ct_param))
    }
    if (site_model$rate_gt_param$estimate == TRUE) {
      text <- c(text, beautier::parameter_to_xml(site_model$rate_gt_param))
    }
  } else if (beautier::is_hky_site_model(site_model)) {
    site_model$kappa_param$id <- id
    text <- c(text, paste0("<parameter id=\"kappa.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">",
      site_model$kappa, "</parameter>"))
  } else if (beautier::is_tn93_site_model(site_model)) {
    if (site_model$kappa_1_param$estimate == TRUE) {
      site_model$kappa_1_param$id <- id
      text <- c(text, beautier::parameter_to_xml(site_model$kappa_1_param))
    }
    if (site_model$kappa_2_param$estimate == TRUE) {
      site_model$kappa_2_param$id <- id
      text <- c(text, beautier::parameter_to_xml(site_model$kappa_2_param))
    }
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
