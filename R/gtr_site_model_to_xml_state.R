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
  testthat::expect_true(beautier::is_site_model(site_model))
  beautier::check_beauti_options(beauti_options)
  id <- site_model$id
  testthat::expect_true(beautier::is_id(id))
  text <- NULL
  testthat::expect_true(beautier::is_gtr_site_model(site_model))

  # Indeed, overwrite by the site model's ID
  site_model$rate_ac_param$id <- id
  site_model$rate_ag_param$id <- id
  site_model$rate_at_param$id <- id
  site_model$rate_cg_param$id <- id
  site_model$rate_ct_param$id <- id
  site_model$rate_gt_param$id <- id
  site_model$freq_param$id <- id

  testthat::expect_true("estimate" %in% names(site_model$rate_ac_param))
  testthat::expect_true("estimate" %in% names(site_model$rate_ag_param))
  testthat::expect_true("estimate" %in% names(site_model$rate_at_param))
  testthat::expect_true("estimate" %in% names(site_model$rate_cg_param))
  testthat::expect_true("estimate" %in% names(site_model$rate_ct_param))
  testthat::expect_true("estimate" %in% names(site_model$rate_gt_param))
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
    beautier::freq_param_to_xml(
      site_model$freq_param,
      beauti_options = beauti_options
    )
  )
  text <- c(
    text,
    beautier::gamma_site_model_to_xml_state(site_model$gamma_site_model, id)
  )
  text
}
