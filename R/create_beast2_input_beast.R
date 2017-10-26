#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_beast <- function(
  input_fasta_filenames,
  site_models = create_site_model(name = "JC69"),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule"),
  mcmc_chainlength = 10000000,
  capitalize_first_char_id = FALSE,
  fixed_crown_age = FALSE,
  initial_phylogeny = NA
) {
  if (!beastscriptr::files_exist(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }

  # Alignment IDs
  ids <- beastscriptr::get_id(
    input_fasta_filenames,
    capitalize_first_char_id = capitalize_first_char_id
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
      capitalize_first_char_id = capitalize_first_char_id
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

  text <- c(text, beastscriptr::create_beast2_input_map())

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
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
