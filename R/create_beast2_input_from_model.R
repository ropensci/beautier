#' Create a BEAST2 XML input text from an inference model
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso
#'   Use \link{create_beast2_input_file_from_model} to also save it to file.
#' @examples
#' library(testthat)
#'
#' text <- create_beast2_input_from_model(
#'   input_filename = get_fasta_filename(),
#'   inference_model = create_inference_model()
#' )
#' expect_true(substr(text[1], 1, 5) == "<?xml")
#' expect_true(tail(text, n = 1) == "</beast>")
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_from_model <- function(
  input_filename,
  inference_model
) {
  if (length(input_filename) != 1) {
    stop("Must use one alignment, site model, clock model and tree prior")
  }
  if (!beautier::files_exist(input_filename)) {
    stop("'input_filename' not found. Value: ", input_filename)
  }
  beautier::check_inference_model(inference_model)

  # Check if the combination of FASTA file and inference model agrees
  check_fasta_file_and_inference_model(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Fill in MRCA prior's taxa names and alignment ID if those are NA
  if (!beautier::is_one_na(inference_model$mrca_prior)) {
    if (beautier::is_one_na(inference_model$mrca_prior$alignment_id)) {
      inference_model$mrca_prior$alignment_id <- get_alignment_id(input_filename) # nolint beautier function
    }
    if (beautier::is_one_na(inference_model$mrca_prior$taxa_names)) {
      inference_model$mrca_prior$taxa_names <- get_taxa_names(input_filename) # nolint beautier function
    }
  }

  # Don't be smart now
  tipdates_filename <- inference_model$tipdates_filename
  site_model <- inference_model$site_model
  clock_model <- inference_model$clock_model
  tree_prior <- inference_model$tree_prior
  mrca_prior <- inference_model$mrca_prior
  mcmc <- inference_model$mcmc
  beauti_options <- inference_model$beauti_options

  # COnvert to lists
  site_models <- list(site_model)
  clock_models <- list(clock_model)
  tree_priors <- list(tree_prior)
  mrca_priors <- list(mrca_prior)
  input_filenames <- input_filename

  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  testit::assert(beautier::is_mcmc(mcmc))
  testit::assert(beautier::is_beauti_options(beauti_options))
  # Lengths
  testit::assert(length(input_filenames) == 1)

  # All MRCA's taxa names must be in the FASTA files
  if (!beautier::is_one_na(mrca_priors)) {
    testit::assert(beautier::are_mrca_priors(mrca_priors))
    if (!beautier::are_mrca_align_ids_in_fastas(
        mrca_priors = mrca_priors,
        fasta_filenames = input_filenames
      )
    ) {
      mrca_ids <- NULL
      for (mrca_prior in mrca_priors) {
        mrca_ids <- paste(mrca_ids, mrca_prior$alignment_id)
      }

      stop(
        paste0(
          "All MRCA prior's alignment IDs must match the FASTA file IDs. ",
          "Use 'get_alignment_id' on the FASTA filename ",
          "to get the correct alignment ID. ",
          "Alignment IDs: ",
            get_alignment_ids_from_fasta_filenames(input_filenames),
          ". MRCA alignment IDs: ", mrca_ids
        )
      )
    }

    if (!beautier::are_mrca_taxa_names_in_fastas(
        mrca_priors = mrca_priors, fasta_filenames = input_filenames
      )
    ) {
      stop("All MRCA prior's taxa names must be FASTA file taxa names")
    }
  }

  if (!beautier::are_mrca_taxa_non_intersecting(mrca_priors)) {
    stop("Monophyletic MRCA priors must have taxon sets without intersection")
  }

  # Initialize all models and priors
  site_models <- init_site_models(
    site_models = site_models,
    ids = get_alignment_ids_from_fasta_filenames(
      fasta_filenames = input_filenames
    ),
    distr_id = 0,
    param_id = 0
  )  # nolint beautier function
  clock_models <- init_clock_models(
    clock_models = clock_models,
    fasta_filenames = input_filenames,
    distr_id = 0 + get_site_models_n_distrs(site_models),
    param_id = 0 + get_site_models_n_params(site_models)
  )  # nolint beautier function
  tree_priors <- init_tree_priors( # nolint beautier function
    tree_priors,
    ids = get_alignment_ids_from_fasta_filenames(
      fasta_filenames = input_filenames
    ),
    distr_id = 100,
    param_id = 200
  )
  mrca_priors <- init_mrca_priors( # nolint beautier function
    mrca_priors,
    distr_id = 150,
    param_id = 300
  )
  testit::assert(beautier::are_init_site_models(site_models))
  testit::assert(beautier::are_init_clock_models(clock_models))
  testit::assert(beautier::are_init_tree_priors(tree_priors))
  testit::assert(beautier::are_init_mrca_priors(mrca_priors))

  # Make a million show as 1000000 instead of 1e+06
  old_scipen <- getOption("scipen")
  options(scipen = 20)

  # Convert from new to older interface
  fixed_crown_ages <- rep(FALSE, times = length(input_filenames))
  initial_phylogenies <- rep(NA, time = length(input_filenames))

  testit::assert(are_initial_phylogenies(initial_phylogenies)) # nolint beautier function
  testit::assert(length(input_filenames) == length(initial_phylogenies)) # nolint beautier function

  text <- create_beast2_input_beast(
    input_filenames = input_filenames,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mrca_priors = mrca_priors,
    mcmc = mcmc,
    beauti_options = beauti_options,
    fixed_crown_ages = fixed_crown_ages,
    initial_phylogenies = initial_phylogenies,
    tipdates_filename = tipdates_filename
  )
  text[1] <- paste0(create_beast2_input_xml(), text[1]) # nolint beautier function

  # Restore scipen
  options(scipen = old_scipen)

  text
}
