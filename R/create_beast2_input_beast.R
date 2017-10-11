#' Creates the beast section of a BEAST2 XML parameter file
#' @param input_fasta_filenames one ore more FASTA filenames
#' @param mcmc_chainlength MCMC chain length
#' @param tree_priors one ore more tree priors,
#'   as returned from 'create_tree_prior'
#' @param fixed_crown_age is the crown age fixed TRUE or FALSE
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_beast <- function(
  input_fasta_filenames,
  mcmc_chainlength,
  tree_priors,
  fixed_crown_age,
  initial_phylogeny
) {

  filename_base <- beastscriptr::remove_file_extension(basename(input_fasta_filenames))
  text <- paste0(
    "<beast beautitemplate='Standard' beautistatus='' ",
    "namespace=\"beast.core:beast.evolution.alignment:",
    "beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:",
    "beast.evolution.operators:beast.evolution.sitemodel:",
    "beast.evolution.substitutionmodel:",
    "beast.evolution.likelihood\" version=\"2.0\">"
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_data(
      filename_base = filename_base,
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
      filename_base = filename_base,
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
