#' Creates the state section of a BEAST2 XML parameter file
#' @param filename_base filename its base
#' @param fasta_filenames the FASTA filename
#' @param mcmc_chainlength MCMC chain length
#' @param tree_priors one or more tree priors,
#'   as returned by 'create_tree_prior'
#' @param fixed_crown_age is the crown age fixed TRUE or FALSE
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_run <- function(
  filename_base,
  fasta_filenames,
  mcmc_chainlength,
  tree_priors,
  fixed_crown_age,
  initial_phylogeny
) {
  text <- NULL

  text <- c(text, paste0("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"",
    mcmc_chainlength, "\">"))

  text <- c(text,
    create_beast2_input_state(
      fasta_filenames = fasta_filenames,
      tree_priors = tree_priors,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")

  text <- c(text,
    create_beast2_input_init(
      fasta_filenames = fasta_filenames,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")

  text <- c(text,
    create_beast2_input_distribution(
      fasta_filenames = fasta_filenames,
      tree_priors = tree_priors
    )
  )

  text <- c(text, "")

  text <- c(text, beastscriptr::create_beast2_input_operators(
    filename_base = filename_base,
    tree_priors = tree_priors,
    fixed_crown_age = fixed_crown_age))

  text <- c(text, "")

  text <- c(text, beastscriptr::create_beast2_input_loggers(
    filename_base = filename_base,
    tree_priors = tree_priors))

  text <- c(text, "")
  text <- c(text, "</run>")
  text
}
