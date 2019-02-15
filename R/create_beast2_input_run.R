#' Creates the '\code{run}' section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filenames using \code{\link{get_alignment_ids}})
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @return lines of XML text
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_run <- function(
  ids,
  site_models = list(create_jc69_site_model(id = ids)),
  clock_models = list(create_strict_clock_model(id = ids)),
  tree_priors = list(create_yule_tree_prior(id = ids)),
  mrca_priors = NA,
  mcmc = create_mcmc(),
  fixed_crown_ages = rep(FALSE, times = length(ids)),
  initial_phylogenies = rep(NA, length(ids)),
  tipdates_filename = NA
) {
  testit::assert(length(ids) == length(initial_phylogenies))
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(length(ids) == length(fixed_crown_ages))
  testit::assert(are_ids(ids))  # nolint beautier function
  testit::assert(are_site_models(site_models)) # nolint beautier function
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  testit::assert(are_tree_priors(tree_priors)) # nolint beautier function
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function

  text <- NULL

  text <- c(text, mcmc_to_xml_run(mcmc)) # nolint beautier function

  text <- c(text,
    indent( # nolint beautier function
      create_beast2_input_state( # nolint beautier function
        site_models = site_models,
        clock_models = clock_models,
        tree_priors = tree_priors,
        initial_phylogenies = initial_phylogenies,
        mrca_priors = mrca_priors,
        tipdates_filename = tipdates_filename
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
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
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
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
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
      mcmc = mcmc,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )

  text <- c(text, "")
  text <- c(text, "</run>")
  text
}
