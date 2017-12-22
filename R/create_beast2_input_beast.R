#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_beast <- function(
  input_fasta_filenames,
  site_models = create_jc69_site_models(ids = get_ids(input_fasta_filenames)),
  clock_models = create_strict_clock_models(
    ids = get_ids(input_fasta_filenames)),
  tree_priors = create_yule_tree_priors(ids = get_ids(input_fasta_filenames)),
  mcmc = create_mcmc(),
  misc_options = create_misc_options(),
  fixed_crown_age = FALSE,
  initial_phylogenies = rep(NA, length(input_fasta_filenames))
) {
  testit::assert(files_exist(input_fasta_filenames)) # nolint internal function
  testit::assert(length(input_fasta_filenames) == length(site_models))
  testit::assert(length(input_fasta_filenames) == length(clock_models))
  testit::assert(length(input_fasta_filenames) == length(tree_priors))
  testit::assert(length(input_fasta_filenames) == length(initial_phylogenies))
  testit::assert(are_site_models(site_models))
  testit::assert(are_clock_models(clock_models))
  testit::assert(are_tree_priors(tree_priors))
  testit::assert(are_init_clock_models(clock_models)) # nolint internal function

  # Alignment IDs
  ids <- beautier::get_id(
    input_fasta_filenames,
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
      input_fasta_filenames = input_fasta_filenames,
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
      mcmc = mcmc,
      tree_priors = tree_priors,
      fixed_crown_ages = rep(fixed_crown_age, times = length(ids)),
      initial_phylogenies = initial_phylogenies
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
