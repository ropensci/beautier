#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_beast <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  testit::assert(length(input_filename) == 1)
  testit::assert(file.exists(input_filename))

  # Do not be smart now
  site_models <- list(inference_model$site_model)
  clock_models <- list(inference_model$clock_model)
  tree_priors <- list(inference_model$tree_prior)
  mrca_priors <- list(inference_model$mrca_prior)
  mcmc <- inference_model$mcmc
  beauti_options <- inference_model$beauti_options
  tipdates_filename <- inference_model$tipdates_filename
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  testit::assert(beautier::are_init_clock_models(clock_models))

  # Alignment IDs
  ids <- beautier::get_alignment_id(
    input_filename,
    capitalize_first_char_id = beauti_options$capitalize_first_char_id
  )

  text <- create_beast2_beast_xml( # nolint beautier function
    beast2_version = beauti_options$beast2_version,
    required = beauti_options$required
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_data(
      input_filenames = input_filename,
      beauti_options = beauti_options
    )
  )

  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ") # Exact same spacing as created by BEAUti
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")

  text <- c(text, create_beast2_input_map()) # nolint beautier function call

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_run(
      ids = ids,
      site_models = site_models,
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      mcmc = mcmc,
      tree_priors = tree_priors,
      fixed_crown_ages = FALSE,
      initial_phylogenies = NA,
      tipdates_filename = tipdates_filename
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
