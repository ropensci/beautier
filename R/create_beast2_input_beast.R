#' Creates the beast section of a BEAST2 XML parameter file
#' @param input_fasta_filenames one ore more FASTA filenames
#' @param site_models one or more site models,
#'   as returned by 'create_site_model'
#' @param mcmc_chainlength MCMC chain length
#' @param tree_priors one ore more tree priors,
#'   as returned from 'create_tree_prior'
#' @param fixed_crown_age is the crown age fixed TRUE or FALSE
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_beast <- function(
  input_fasta_filenames,
  site_models = create_site_model(name = "JC69"),
  tree_priors = create_tree_prior(name = "yule"),
  mcmc_chainlength,
  fixed_crown_age,
  initial_phylogeny
) {
  if (!file.exists(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }

  # Alignment IDs
  ids <- beastscriptr::get_file_base_sans_ext(input_fasta_filenames)

  text <- paste0(
    "<beast beautitemplate='Standard' beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood\" ",
    "required=\"\" version=\"2.4\">"
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_data(
      input_fasta_filenames = input_fasta_filenames
    )
  )

  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
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
