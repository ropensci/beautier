#' Creates the operators section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_operators <- function(
  inference_model
) {
  # Do not be smart yet
  site_models <- list(inference_model$site_model)

  text <- beautier::tree_prior_to_xml_operators(
    inference_model = inference_model
  )

  text <- c(text, beautier::site_models_to_xml_operators(site_models))
  text <- c(
    text,
    beautier::clock_model_to_xml_operators(
      inference_model
    )
  )
  text <- beautier::interspace(text)
  text
}
