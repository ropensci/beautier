#' Creates the '\code{run}' section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filenames
#'   using \code{\link{get_alignment_ids_from_fasta_filenames}})
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @return lines of XML text
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_run <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  testit::assert(length(input_filename) == 1)

  # Alignment IDs
  ids <- beautier::get_alignment_id(
    input_filename,
    capitalize_first_char_id =
      inference_model$beauti_options$capitalize_first_char_id
  )

  # Do not be smart yet
  site_models <- list(inference_model$site_model)
  clock_models <- list(inference_model$clock_model)
  tree_priors <- list(inference_model$tree_prior)
  mrca_priors <- list(inference_model$mrca_prior)
  mcmc <- inference_model$mcmc
  fixed_crown_ages <- FALSE
  initial_phylogenies <- NA
  tipdates_filename <- inference_model$tipdates_filename

  testit::assert(length(ids) == length(initial_phylogenies))
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(length(ids) == length(fixed_crown_ages))
  testit::assert(beautier::are_ids(ids))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(beautier::are_mrca_priors(mrca_priors))

  text <- NULL

  text <- c(text, mcmc_to_xml_run(mcmc)) # nolint beautier function

  text <- c(text,
    beautier::indent(
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
