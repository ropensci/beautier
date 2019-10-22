#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_beast <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  # Do not be smart now
  input_filenames <- input_filename
  site_models <- list(inference_model$site_model)
  clock_models <- list(inference_model$clock_model)
  tree_priors <- list(inference_model$tree_prior)
  mrca_priors <- list(inference_model$mrca_prior)
  mcmc <- inference_model$mcmc
  beauti_options <- inference_model$beauti_options
  tipdates_filename <- inference_model$tipdates_filename
  initial_phylogenies <- NA
  fixed_crown_ages <- FALSE
  testit::assert(files_exist(input_filenames)) # nolint beautier function
  testit::assert(length(input_filenames) == length(site_models))
  testit::assert(length(input_filenames) == length(clock_models))
  testit::assert(length(input_filenames) == length(tree_priors))
  testit::assert(length(input_filenames) == length(initial_phylogenies))
  testit::assert(length(input_filenames) == length(fixed_crown_ages))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  testit::assert(beautier::are_init_clock_models(clock_models))
  testit::assert(beautier::are_initial_phylogenies(initial_phylogenies))

  # Alignment IDs
  ids <- beautier::get_alignment_id(
    input_filenames,
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
      input_filenames = input_filenames,
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
      fixed_crown_ages = fixed_crown_ages,
      initial_phylogenies = initial_phylogenies,
      tipdates_filename = tipdates_filename
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
