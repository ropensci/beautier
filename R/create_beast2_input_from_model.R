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
  beautier::check_inference_model(inference_model)

  # Don't be smart now
  tipdates_filename <- inference_model$tipdates_filename
  site_model <- inference_model$site_model
  clock_model <- inference_model$clock_model
  tree_prior <- inference_model$tree_prior
  mrca_prior <- inference_model$mrca_prior
  mcmc <- inference_model$mcmc
  beauti_options <- inference_model$beauti_options


  # 2 site_models
  check_site_model(site_model) # nolint beautier function
  site_models <- site_model

  # Convert possible-non-list input to lists and multiPhylo
  if (is_site_model(site_models)) { # nolint beautier function
    site_models <- list(site_models)
  }
  clock_models <- clock_model
  if (is_clock_model(clock_models)) { # nolint beautier function
    clock_models <- list(clock_models)
  }
  tree_priors <- tree_prior
  if (is_tree_prior(tree_priors)) { # nolint beautier function
    tree_priors <- list(tree_priors)
  }
  mrca_priors <- mrca_prior
  if (is_mrca_prior(mrca_priors)) { # nolint beautier function
    mrca_priors <- list(mrca_priors)
    testit::assert(is_mrca_prior(mrca_priors[[1]])) # nolint beautier function
  }

  # Check input
  # 1 input_filenames
  input_filenames <- input_filename
  if (!files_exist(input_filenames)) { # nolint beautier function
    stop("'input_filename' not found. Value: ", input_filenames)
  }

  # 2 site_models
  # Already checked

  # 3 clock_models
  if (!beautier::are_clock_models(clock_models)) {
    stop(
      "'clock_model' must be a valid clock model, ",
      "as returned by 'create_clock_model'"
    )
  }

  # 4 tree_priors
  if (!beautier::are_tree_priors(tree_priors)) {
    stop(
      "'tree_prior' must be a valid tree prior, ",
      "as returned by 'create_tree_prior'"
    )
  }

  # 5 MRCA priors
  if (!beautier::are_mrca_priors(mrca_priors)) {
    stop(
      "'mrca_prior' must be NA or a valid mrca object, ",
      "as returned by 'create_mrca_prior'"
    )
  }

  # 6 mcmc
  if (!beautier::is_mcmc(mcmc)) {
    stop(
      "'mcmc' must be a valid mcmc object, ",
      "as returned by 'create_mcmc'"
    )
  }

  # 7 beauti_options
  if (!beautier::is_beauti_options(beauti_options)) {
    stop(
      "'beauti_options' must be a valid 'beauti_options', ",
      "as returned by 'create_beauti_options'"
    )
  }

  # Lengths
  if (length(input_filenames) != 1) {
    stop("Must use one alignment, site model, clock model and tree prior")
  }

  # Higher-level checks
  for (i in seq_along(input_filenames)) {
    fasta_filename <- input_filenames[i]
    tree_prior <- tree_priors[[i]]
    if (is_cbs_tree_prior(tree_prior)) { # nolint beautier function
      n_taxa <- get_n_taxa(fasta_filename) # nolint beautier function
      group_sizes_dimension <- tree_prior$group_sizes_dimension
      if (n_taxa <= group_sizes_dimension) {
        stop(
          "'group_sizes_dimension' (", group_sizes_dimension,
          ") must be less than the number of taxa (", n_taxa, ")"
        )
      }
    }
  }

  # Fill in MRCA prior's taxa names and alignment ID if those are NA
  if (!is_one_na(mrca_priors[[1]])) { # nolint beautier function
    for (i in seq_along(mrca_priors)) {
      if (is_one_na(mrca_priors[[i]]$alignment_id)) { # nolint beautier function
        mrca_priors[[i]]$alignment_id <- get_alignment_id(input_filename) # nolint beautier function
      }
      if (is_one_na(mrca_priors[[i]]$taxa_names)) { # nolint beautier function
        mrca_priors[[i]]$taxa_names <- get_taxa_names(input_filename) # nolint beautier function
      }
    }
  }

  # All MRCA's taxa names must be in the FASTA files
  if (!is_one_na(mrca_priors)) { # nolint beautier function
    testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
    if (!are_mrca_align_ids_in_fastas( # nolint beautier function
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

    if (!are_mrca_taxa_names_in_fastas(
        mrca_priors = mrca_priors, fasta_filenames = input_filenames
      )
    ) {
      stop("All MRCA prior's taxa names must be FASTA file taxa names")
    }
  }

  if (!are_mrca_taxa_non_intersecting(mrca_priors = mrca_priors)) {
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
  testit::assert(are_init_site_models(site_models))  # nolint beautier function
  testit::assert(are_init_clock_models(clock_models))  # nolint beautier function
  testit::assert(are_init_tree_priors(tree_priors))  # nolint beautier function
  testit::assert(are_init_mrca_priors(mrca_priors))  # nolint beautier function

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
