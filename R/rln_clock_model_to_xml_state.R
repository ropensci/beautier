#' Internal function
#'
#' Converts an RLN clock model to the `state` section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
rln_clock_model_to_xml_state <- function(
  inference_model
) {
  beautier::check_inference_model(inference_model)

  # Don't be smart yet
  clock_model <- inference_model$clock_model

  text <- NULL
  # Fails on unimplemented clock models
  testthat::expect_true(beautier::is_rln_clock_model(clock_model))
  testthat::expect_false(beautier::is_one_na(clock_model$mean_clock_rate))
  testthat::expect_false(beautier::is_one_na(clock_model$dimension))

  if (beautier::has_mrca_prior_with_distr(inference_model) ||
      beautier::has_tip_dating(inference_model)
  ) {
    text <- c(
      text,
      beautier::create_ucld_mean_state_node_param_xml(inference_model)
    )
  }
  text <- c(
    text,
    beautier::create_ucld_stdev_state_node_param_xml(inference_model)
  )
  text <- c(
    text,
    beautier::create_rate_categories_state_node_xml(inference_model)
  )
  text
}
