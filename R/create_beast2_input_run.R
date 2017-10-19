#' Creates the state section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using 'get_file_base_sans_ext')
#' @param site_models one or more site models, as returned
#'   by 'create_site_model'
#' @param tree_priors one or more tree priors,
#'   as returned by 'create_tree_prior'
#' @param mcmc_chainlength MCMC chain length
#' @param fixed_crown_age is the crown age fixed TRUE or FALSE
#' @param initial_phylogeny initial phylogeny or NA
#' @export
create_beast2_input_run <- function(
  ids,
  site_models = create_site_model(name = "JC69"),
  mcmc_chainlength,
  tree_priors = create_tree_prior(name = "yule"),
  fixed_crown_age,
  initial_phylogeny
) {
  text <- NULL

  text <- c(text, paste0("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"",
    mcmc_chainlength, "\">"))

  text <- c(text,
    create_beast2_input_state(
      ids = ids,
      site_models = site_models,
      tree_priors = tree_priors,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")

  text <- c(text,
    create_beast2_input_init(
      ids = ids,
      initial_phylogeny = initial_phylogeny
    )
  )

  text <- c(text, "")

  text <- c(text,
    create_beast2_input_distribution(
      ids = ids,
      site_models = site_models,
      tree_priors = tree_priors
    )
  )

  text <- c(text, "")

  text <- c(text, beastscriptr::create_beast2_input_operators(
    ids = ids,
    tree_priors = tree_priors,
    fixed_crown_age = fixed_crown_age))

  text <- c(text, "")

  text <- c(text, beastscriptr::create_beast2_input_loggers(
    ids = ids,
    tree_priors = tree_priors))

  text <- c(text, "")
  text <- c(text, "</run>")
  text
}
