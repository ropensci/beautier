#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_beast <- function(
  input_filenames,
  site_models = create_jc69_site_models(ids = get_ids(input_filenames)),
  clock_models = create_strict_clock_models(
    ids = get_ids(input_filenames)),
  tree_priors = create_yule_tree_priors(ids = get_ids(input_filenames)),
  mrca_priors = NA,
  mcmc = create_mcmc(),
  misc_options = create_misc_options(),
  fixed_crown_ages = rep(FALSE, times = length(input_filenames)),
  initial_phylogenies = rep(NA, length(input_filenames))
) {
  testit::assert(files_exist(input_filenames)) # nolint internal function
  testit::assert(length(input_filenames) == length(site_models))
  testit::assert(length(input_filenames) == length(clock_models))
  testit::assert(length(input_filenames) == length(tree_priors))
  testit::assert(length(input_filenames) == length(initial_phylogenies))
  testit::assert(length(input_filenames) == length(fixed_crown_ages))
  testit::assert(are_site_models(site_models))
  testit::assert(are_clock_models(clock_models))
  testit::assert(are_tree_priors(tree_priors))
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function
  testit::assert(are_init_clock_models(clock_models)) # nolint internal function
  testit::assert(are_initial_phylogenies(initial_phylogenies)) # nolint internal function

  # Alignment IDs
  ids <- beautier::get_id(
    input_filenames,
    capitalize_first_char_id = misc_options$capitalize_first_char_id
  )

  text <- paste0(
    "<beast beautitemplate='Standard' beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
    "beast.evolution.tree.coalescent:beast.core.util:",
    "beast.evolution.nuc:beast.evolution.operators:",
    "beast.evolution.sitemodel:",
    "beast.evolution.substitutionmodel:",
    "beast.evolution.likelihood\" ",
    "required=\"\" version=\"2.4\">"
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
      initial_phylogenies = initial_phylogenies
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
