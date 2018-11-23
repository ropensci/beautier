#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
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
  misc_options = create_misc_options(),
  fixed_crown_ages = rep(FALSE, times = length(input_filenames)),
  initial_phylogenies = rep(NA, length(input_filenames)),
  has_tip_dating = FALSE
) {
  testit::assert(files_exist(input_filenames)) # nolint internal function
  testit::assert(length(input_filenames) == length(site_models))
  testit::assert(length(input_filenames) == length(clock_models))
  testit::assert(length(input_filenames) == length(tree_priors))
  testit::assert(length(input_filenames) == length(initial_phylogenies))
  testit::assert(length(input_filenames) == length(fixed_crown_ages))
  testit::assert(are_site_models(site_models)) # nolint internal function
  testit::assert(are_clock_models(clock_models)) # nolint internal function
  testit::assert(are_tree_priors(tree_priors)) # nolint internal function
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function
  testit::assert(are_init_clock_models(clock_models)) # nolint internal function
  testit::assert(are_initial_phylogenies(initial_phylogenies)) # nolint internal function

  # Alignment IDs
  ids <- beautier::get_alignment_id(
    input_filenames,
    capitalize_first_char_id = misc_options$capitalize_first_char_id
  )

  text <- create_beast2_beast_xml( # nolint internal function
    beast2_version = misc_options$beast2_version,
    required = misc_options$required
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_data(
      input_filenames = input_filenames,
      misc_options = misc_options
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

  text <- c(text, create_beast2_input_map()) # nolint internal function call

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
      has_tip_dating = has_tip_dating
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
