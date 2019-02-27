#' Create a BEAST2 XML input text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso
#'   Use \link{create_beast2_input_file} to also save it to file.
#'   Use \link{create_beast2_input_file_from_model} to use an inference model
#'     as an input argument.
#' @examples
#'   text <- create_beast2_input(
#'     input_filename = get_fasta_filename()
#'   )
#'   testit::assert(substr(text[1], 1, 5) == "<?xml")
#'   text[1]
#'   testit::assert(tail(text, n = 1) == "</beast>")
#' @seealso \code{\link{create_beast2_input_file}} shows more examples
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input <- function(
  input_filename,
  tipdates_filename = NA,
  site_model = create_jc69_site_model(),
  clock_model = create_strict_clock_model(),
  tree_prior = create_yule_tree_prior(),
  mrca_prior = NA,
  mcmc = create_mcmc(),
  beauti_options = create_beauti_options(),
  input_filenames = "deprecated",
  site_models = "deprecated",
  clock_models = "deprecated",
  tree_priors = "deprecated",
  mrca_priors = "deprecated",
  posterior_crown_age = "deprecated"
) {
  # Check for deprecated argument names
  calls <- names(sapply(match.call(), deparse))[-1]
  if (any("posterior_crown_age" %in% calls)) {
    stop(
      "'posterior_crown_age' is deprecated. \n",
      "Tip: use an MRCA prior ",
      "with a narrow distribution around the crown age instead. \n",
      "See 'create_mrca_prior' or the example below:\n",
      "\n",
      "fasta_filename <- get_beautier_path(\"anthus_aco.fas\")\n",
      "crown_age <- 15\n",
      "\n",
      "mrca_prior <- create_mrca_prior(\n",
      "  alignment_id = get_alignment_id(fasta_filename = fasta_filename),\n",
      "  taxa_names = get_taxa_names(filename = fasta_filename),\n",
      "  mrca_distr = create_normal_distr(\n",
      "    mean = crown_age,\n",
      "    sigma = 0.0001\n",
      "  ),\n",
      "  is_monophyletic = TRUE\n",
      ")\n",
      "\n",
      "create_beast2_input(\n",
      "  input_filename = fasta_filename,\n",
      "  mrca_prior = mrca_prior\n",
      ")\n"
    )
  }
  if (any("input_filenames" %in% calls)) {
    stop("'input_filenames' is deprecated, use 'input_filename' instead.")
  }
  if (any("site_models" %in% calls)) {
    stop("'site_models' is deprecated, use 'site_model' instead.")
  }
  if (any("clock_models" %in% calls)) {
    stop("'clock_models' is deprecated, use 'clock_model' instead.")
  }
  if (any("tree_priors" %in% calls)) {
    stop("'tree_priors' is deprecated, use 'tree_prior' instead.")
  }
  if (any("mrca_priors" %in% calls)) {
    stop("'mrca_priors' is deprecated, use 'mrca_prior' instead.")
  }

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
  if (!are_clock_models(clock_models)) { # nolint beautier function
    stop(
      "'clock_model' must be a valid clock model, ",
      "as returned by 'create_clock_model'"
    )
  }

  # 4 tree_priors
  if (!are_tree_priors(tree_priors)) { # nolint beautier function
    stop(
      "'tree_prior' must be a valid tree prior, ",
      "as returned by 'create_tree_prior'"
    )
  }

  # 5 MRCA priors
  if (!are_mrca_priors(mrca_priors)) { # nolint beautier function
    stop(
      "'mrca_prior' must be NA or a valid mrca object, ",
      "as returned by 'create_mrca_prior'"
    )
  }

  # 6 mcmc
  if (!is_mcmc(mcmc)) { # nolint beautier function
    stop(
      "'mcmc' must be a valid mcmc object, ",
      "as returned by 'create_mcmc'"
    )
  }

  # 7 beauti_options
  if (!is_beauti_options(beauti_options)) { # nolint beautier function
    stop(
      "'beauti_options' must be a valid misc options object, ",
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
          "Alignment IDs: ", get_alignment_ids(input_filenames),
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
    ids = get_alignment_ids(input_filenames),
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
    ids = get_alignment_ids(input_filenames),
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
