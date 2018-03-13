#' Create a BEAST2 XML input text
#' @inheritParams default_params_doc
#' @examples
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "my_beast.xml"
#'   )
#' @seealso \code{\link{create_beast2_input_file}} shows more examples
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input <- function(
  input_filenames,
  site_models = create_jc69_site_models(ids = get_ids(input_filenames)),
  clock_models = create_strict_clock_models(
    ids = get_ids(input_filenames)),
  tree_priors = create_yule_tree_priors(ids = get_ids(input_filenames)),
  mrca_priors = NA,
  mcmc = create_mcmc(),
  misc_options = create_misc_options(),
  posterior_crown_age = NA
) {
  # Convert possible-non-list input to lists and multiPhylo
  if (is_site_model(site_models)) {
    site_models <- list(site_models)
  }
  if (is_clock_model(clock_models)) {
    clock_models <- list(clock_models)
  }
  if (is_tree_prior(tree_priors)) {
    tree_priors <- list(tree_priors)
  }
  if (is_mrca_prior(mrca_priors)) {
    mrca_priors <- list(mrca_priors)
    testit::assert(is_mrca_prior(mrca_priors[[1]]))
  }

  # Check input

  # 1 input_filenames
  if (!files_exist(input_filenames)) {
    stop(
      "'input_filenames' must be the name ",
      "of one or more present files. "
    )
  }

  # 2 site_models
  if (!are_site_models(site_models)) {
    stop(
      "'site_models' must be a valid site model, ",
      "or a list of valid site models, ",
      "as returned by 'create_site_model'"
    )
  }

  # 3 clock_models
  if (!are_clock_models(clock_models)) {
    stop(
      "'clock_models' must be a valid clock model, ",
      "or a list of valid clock models, ",
      "as returned by 'create_clock_model'"
    )
  }

  # 4 tree_priors
  if (!are_tree_priors(tree_priors)) {
    stop(
      "'tree_priors' must be a valid tree prior, ",
      "or a list of valid tree priors, ",
      "as returned by 'create_tree_prior'"
    )
  }

  # 5 MRCA priors
  if (!are_mrca_priors(mrca_priors)) {
    stop(
      "'mrca_priors' must be NA or a valid mrca object, ",
      "as returned by 'create_mrca_prior'"
    )
  }

  # 6 mcmc
  if (!is_mcmc(mcmc)) {
    stop(
      "'mcmc' must be a valid mcmc object, ",
      "as returned by 'create_mcmc'"
    )
  }

  # 7 misc_options
  if (!is_misc_options(misc_options)) {
    stop(
      "'misc_options' must be a valid misc options object, ",
      "as returned by 'create_misc_options'"
    )
  }

  if (!is.na(posterior_crown_age) && !is.numeric(posterior_crown_age)) {
    stop("'posterior_crown_age' must be either NA or a non-zero postive value")
  }
  if (!is.na(posterior_crown_age) && posterior_crown_age <= 0.0) {
    stop("'posterior_crown_age' must be either NA or a non-zero postive value")
  }

  # Lengths
  if (length(input_filenames) != length(site_models)) {
    stop("Must supply as much input_filenames as site_models")
  }
  if (length(input_filenames) != length(clock_models)) {
    stop("Must supply as much input_filenames as clock_models")
  }
  if (length(input_filenames) != length(tree_priors)) {
    stop("Must supply as much input_filenames as tree priors")
  }

  # Higher-level checks
  for (i in seq_along(input_filenames)) {
    fasta_filename <- input_filenames[i]
    tree_prior <- tree_priors[[i]]
    if (is_cbs_tree_prior(tree_prior)) {
      n_taxa <- get_n_taxa(fasta_filename)
      group_sizes_dimension <- tree_prior$group_sizes_dimension
      if (n_taxa <= group_sizes_dimension) {
        stop(
          "'group_sizes_dimension' (", group_sizes_dimension,
          ") must be less than the number of taxa (", n_taxa, ")"
        )
      }
    }
  }

  # All MRCA's taxa names must be in the FASTA files

  if (!is_one_na(mrca_priors)) {
    testit::assert(are_mrca_priors(mrca_priors))
    if (!are_mrca_align_ids_in_fastas(
        mrca_priors = mrca_priors, fasta_filenames = input_filenames
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
    ids = get_ids(input_filenames),
    distr_id = 0,
    param_id = 0
  )  # nolint internal function
  clock_models <- init_clock_models(
    clock_models = clock_models,
    fasta_filenames = input_filenames,
    distr_id = 0 + get_site_models_n_distrs(site_models),
    param_id = 0 + get_site_models_n_params(site_models)
  )  # nolint internal function
  tree_priors <- init_tree_priors( # nolint internal function
    tree_priors,
    ids = get_ids(input_filenames),
    distr_id = 100,
    param_id = 200
  )
  mrca_priors <- init_mrca_priors( # nolint internal function
    mrca_priors,
    distr_id = 150,
    param_id = 300
  )
  testit::assert(are_init_site_models(site_models))  # nolint internal function
  testit::assert(are_init_clock_models(clock_models))  # nolint internal function
  testit::assert(are_init_tree_priors(tree_priors))  # nolint internal function
  testit::assert(are_init_mrca_priors(mrca_priors))  # nolint internal function

  # More complex
  if (has_shared_rln_clock_models(clock_models)) {
    stop("Cannot have shared Relaxed Log-Normal clock models")
  }

  # Make a million show as 1000000 instead of 1e+06
  options(scipen = 20)

  # Convert from new to older interface
  fixed_crown_ages <- rep(!is.na(posterior_crown_age),
    times = length(input_filenames))
  initial_phylogenies <- rep(NA, time = length(input_filenames))
  if (!is.na(posterior_crown_age)) {
    initial_phylogenies <- fastas_to_phylos(
      fasta_filenames = input_filenames,
      crown_age = posterior_crown_age
    )
  }
  testit::assert(are_initial_phylogenies(initial_phylogenies)) # nolint internal function
  testit::assert(length(input_filenames) == length(initial_phylogenies)) # nolint internal function

  text <- create_beast2_input_beast(
    input_filenames = input_filenames,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mrca_priors = mrca_priors,
    mcmc = mcmc,
    misc_options = misc_options,
    fixed_crown_ages = fixed_crown_ages,
    initial_phylogenies = initial_phylogenies
  )
  text[1] <- paste0(create_beast2_input_xml(), text[1]) # nolint internal function

  text
}
