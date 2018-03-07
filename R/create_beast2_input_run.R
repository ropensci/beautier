#' Creates the state section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_run <- function(
  ids,
  site_models = create_jc69_site_models(ids = ids),
  clock_models = create_strict_clock_models(ids = ids),
  tree_priors = create_yule_tree_priors(ids = ids),
  mrca_priors = NA,
  mcmc = create_mcmc(),
  fixed_crown_ages = rep(FALSE, times = length(ids)),
  initial_phylogenies = rep(NA, length(ids))
) {
  testit::assert(length(ids) == length(initial_phylogenies))
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(length(ids) == length(fixed_crown_ages))
  testit::assert(are_ids(ids))  # nolint internal function
  testit::assert(are_site_models(site_models))
  testit::assert(are_clock_models(clock_models))
  testit::assert(are_tree_priors(tree_priors))
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function

  text <- NULL

  text <- c(text, mcmc_to_xml_run(mcmc)) # nolint internal function

  text <- c(text,
    indent(
      create_beast2_input_state(
        site_models = site_models,
        clock_models = clock_models,
        tree_priors = tree_priors,
        initial_phylogenies = initial_phylogenies,
        mrca_priors = mrca_priors
      ),
      n_spaces = 4
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
    create_beast2_input_distr(
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      mrca_priors = mrca_priors
    )
  )

  text <- c(text, "")

  text <- c(
    text,
    create_beast2_input_operators(
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      fixed_crown_ages = fixed_crown_ages,
      mrca_priors = mrca_priors
    )
  )

  text <- c(text, "")

  text <- c(
    text,
    create_beast2_input_loggers(
      ids = ids,
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      mrca_priors = mrca_priors
    )
  )

  text <- c(text, "")
  text <- c(text, "</run>")
  text
}
