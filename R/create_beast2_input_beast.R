#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_beast <- function(
  input_fasta_filenames,
  site_models = create_jc69_site_models(n = length(input_fasta_filenames)),
  clock_models = create_strict_clock_models(n = length(input_fasta_filenames)),
  tree_priors = create_yule_tree_priors(n = length(input_fasta_filenames)),
  mcmc_chainlength = 10000000,
  misc_options = create_misc_options(),
  fixed_crown_age = FALSE,
  initial_phylogenies = rep(NA, length(input_fasta_filenames))
) {
  if (!beautier::files_exist(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }
  if (is_site_model(site_models)) {
    site_models <- list(site_models)
  }
  if (is_clock_model(clock_models)) {
    clock_models <- list(clock_models)
  }
  if (is_tree_prior(tree_priors)) {
    tree_priors <- list(tree_priors)
  }
  if (length(input_fasta_filenames) != length(site_models)) {
    stop("Must supply as much FASTA filenames (",
      length(input_fasta_filenames), "), as site_model objects (",
      length(site_models), ")")
  }
  if (length(input_fasta_filenames) != length(clock_models)) {
    stop("Must supply as much FASTA filenames (",
      length(input_fasta_filenames), "), as clock_model objects (",
      length(clock_models), ")")
  }
  if (length(input_fasta_filenames) != length(tree_priors)) {
    stop("Must supply as much FASTA filenames (",
      length(input_fasta_filenames), "), as tree_prior objects (",
      length(tree_priors), ")")
  }
  if (length(input_fasta_filenames) != length(initial_phylogenies)) {
    stop("Must supply as much input_fasta_filenames as initial_phylogenies")
  }

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

  text <- c(text, beautier::create_beast2_input_map())

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_run(
      ids = ids,
      site_models = site_models,
      clock_models = clock_models,
      mcmc_chainlength = mcmc_chainlength,
      tree_priors = tree_priors,
      fixed_crown_age = fixed_crown_age,
      initial_phylogenies = initial_phylogenies
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
