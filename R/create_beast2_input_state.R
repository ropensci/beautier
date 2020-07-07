#' Creates the state section of a BEAST2 XML parameter file
#' @return lines of XML text
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_state <- function(
  inference_model,
  site_models = "deprecated",
  clock_models = "deprecated",
  tree_priors = "deprecated",
  mrca_priors = "deprecated",
  tipdates_filename = "deprecated"
) {
  if (site_models != "deprecated") {
    stop("'site_models' is deprecated, use 'inference_model' instead")
  }
  if (clock_models != "deprecated") {
    stop("'clock_models' is deprecated, use 'inference_model' instead")
  }
  if (tree_priors != "deprecated") {
    stop("'tree_priors' is deprecated, use 'inference_model' instead")
  }
  if (mrca_priors != "deprecated") {
    stop("'mrca_priors' is deprecated, use 'inference_model' instead")
  }
  if (tipdates_filename != "deprecated") {
    stop("'tipdates_filename' is deprecated, use 'inference_model' instead")
  }

  # Do not be smart yet
  site_models <- list(inference_model$site_model)
  clock_models <- list(inference_model$clock_model)
  tree_priors <- list(inference_model$tree_prior)
  mrca_priors <- list(inference_model$mrca_prior)
  tipdates_filename <- inference_model$tipdates_filename

  testit::assert(beautier::are_tree_priors(tree_priors))
  has_tip_dating <- !beautier::is_one_na(tipdates_filename)

  text <- NULL
  text <- c(
    text,
    beautier::phylo_to_xml_state(
      id = inference_model$tree_prior$id,
      inference_model = inference_model
    )
  )
  text <- c(text, beautier::site_models_to_xml_state(site_models))
  text <- c(
    text,
    beautier::clock_models_to_xml_state(
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      has_tip_dating = has_tip_dating
    )
  )
  text <- c(text, beautier::tree_priors_to_xml_state(tree_priors))
  text <- c(
    text,
    beautier::mrca_priors_to_xml_state(
      mrca_priors,
      has_non_strict_clock_model = beautier::get_has_non_strict_clock_model(
        clock_models
      )
    )
  )

  text <- beautier::indent(text)

  if (inference_model$beauti_options$beast2_version == "2.6") {
    text <- c("<state id=\"state\" spec=\"State\" storeEvery=\"5000\">", text)
  } else {
    text <- c("<state id=\"state\" storeEvery=\"5000\">", text)
  }

  text <- c(text, "</state>")
  text
}
