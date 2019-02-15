#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_beast <- function(
  input_filenames,
  site_models = list(
    create_jc69_site_model(
      id = get_alignment_id(input_filenames)
    )
  ),
  clock_models = list(
    create_strict_clock_model(
      id = get_alignment_id(input_filenames)
    )
  ),
  tree_priors = list(
    create_yule_tree_prior(
      id = get_alignment_id(input_filenames)
    )
  ),
  mrca_priors = NA,
  mcmc = create_mcmc(),
  beauti_options = create_beauti_options(),
  fixed_crown_ages = rep(FALSE, times = length(input_filenames)),
  initial_phylogenies = rep(NA, length(input_filenames)),
  tipdates_filename = NA
) {
  testit::assert(files_exist(input_filenames)) # nolint beautier function
  testit::assert(length(input_filenames) == length(site_models))
  testit::assert(length(input_filenames) == length(clock_models))
  testit::assert(length(input_filenames) == length(tree_priors))
  testit::assert(length(input_filenames) == length(initial_phylogenies))
  testit::assert(length(input_filenames) == length(fixed_crown_ages))
  testit::assert(are_site_models(site_models)) # nolint beautier function
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  testit::assert(are_tree_priors(tree_priors)) # nolint beautier function
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  testit::assert(are_init_clock_models(clock_models)) # nolint beautier function
  testit::assert(are_initial_phylogenies(initial_phylogenies)) # nolint beautier function

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
