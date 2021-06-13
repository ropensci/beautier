#' Internal function
#'
#' Creates the XML of a Yule tree prior,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
yule_tree_prior_to_xml_operators <- function( # nolint indeed a long function name
  inference_model
) {
  # Don't be smart yet
  tree_prior <- inference_model$tree_prior
  id <- tree_prior$id
  testthat::expect_true(beautier::is_yule_tree_prior(tree_prior))
  testthat::expect_true(beautier::is_id(id))

  yule_birth_rate_scaler_xml <- paste0(
    "<operator id=\"YuleBirthRateScaler.t:", id, "\" ",
    "spec=\"ScaleOperator\" parameter=\"@birthRate.t:", id, "\" "
  )
  # Add scale factor if:
  # * version != 2.6
  # * version == 2.6 && tipdates
  # * version == 2.6 && RLN
  add_scale_factor <- TRUE

  if (inference_model$beauti_options$beast2_version == "2.6" &&
    !beautier::is_rln_clock_model(inference_model$clock_model) &&
    beautier::has_tip_dating(inference_model)
  ) {
    add_scale_factor <- FALSE
  }
  if (inference_model$beauti_options$beast2_version == "2.6" &&
      beautier::is_rln_clock_model(inference_model$clock_model) &&
      !beautier::has_tip_dating(inference_model)
  ) {
    add_scale_factor <- FALSE
  }
  if (add_scale_factor) {
    yule_birth_rate_scaler_xml <- paste0(
      yule_birth_rate_scaler_xml, "scaleFactor=\"0.75\" "
    )
  }

  yule_birth_rate_scaler_xml <- paste0(
    yule_birth_rate_scaler_xml,
    "weight=\"3.0\"/>"
  )
  yule_birth_rate_scaler_xml
}
