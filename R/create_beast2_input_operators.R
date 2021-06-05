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
  clock_models <- list(inference_model$clock_model)
  tree_priors <- list(inference_model$tree_prior)
  mrca_priors <- list(inference_model$mrca_prior)
  tipdates_filename <- inference_model$tipdates_filename
  fixed_crown_ages <- FALSE

  testit::assert(beautier::is_one_bool(fixed_crown_ages))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  testit::assert(length(site_models) == length(fixed_crown_ages))

  text <- NULL


  text <- c(
    text,
    beautier::tree_prior_to_xml_operators(
      inference_model = inference_model
    )
  )

  text <- c(text, beautier::site_models_to_xml_operators(site_models))
  text <- c(
    text,
    beautier::clock_models_to_xml_operators(
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )
  text <- beautier::interspace(text)
  text
}
