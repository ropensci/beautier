#' Creates the state section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_run <- function(
  ids,
  site_models = create_jc69_site_models(n = length(ids)),
  clock_models = create_strict_clock_models(n = length(ids)),
  tree_priors = create_yule_tree_priors(n = length(ids)),
  mcmc_chainlength = 10000000,
  fixed_crown_age = FALSE,
  initial_phylogenies = rep(NA, length(ids))
) {
  if (length(ids) != length(initial_phylogenies)) {
    stop("Must supply as much IDs as initial_phylogenies")
  }
  if (length(ids) != length(site_models)) {
    stop("Must supply as much IDs as site_model objects")
  }
  if (length(ids) != length(clock_models)) {
    stop("Must supply as much IDs as clock_model objects")
  }
  if (length(ids) != length(tree_priors)) {
    stop("Must supply as much IDs as tree_prior objects")
  }


  text <- NULL

  text <- c(text, paste0("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"",
    mcmc_chainlength, "\">"))

  text <- c(text,
    create_beast2_input_state(
      ids = ids,
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      initial_phylogenies = initial_phylogenies
    )
  )

  text <- c(text,
    create_beast2_input_init(
      ids = ids,
      initial_phylogenies = initial_phylogenies
    )
  )

  text <- c(text, "")

  text <- c(text,
    create_beast2_input_distribution(
      ids = ids,
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors
    )
  )

  text <- c(text, beautier::create_beast2_input_operators(
    ids = ids,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    fixed_crown_age = fixed_crown_age))

  text <- c(text, "")

  text <- c(text, beautier::create_beast2_input_loggers(
    ids = ids,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors))

  text <- c(text, "")
  text <- c(text, "</run>")
  text
}
