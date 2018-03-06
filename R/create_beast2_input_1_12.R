#' Create a BEAST2 XML input text, interface of v1.12
#' @inheritParams default_params_doc
#' @examples
#'   create_beast2_input_file_1_12(
#'     input_filenames = get_fasta_filename(),
#'     "my_beast.xml"
#'   )
#' @seealso \code{\link{create_beast2_input_file}} shows more examples
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_1_12 <- function(
  input_filenames,
  site_models = create_jc69_site_models(ids = get_ids(input_filenames)),
  clock_models = create_strict_clock_models(
    ids = get_ids(input_filenames)),
  tree_priors = create_yule_tree_priors(ids = get_ids(input_filenames)),
  mcmc = create_mcmc(),
  misc_options = create_misc_options(),
  fixed_crown_ages = rep(FALSE, times = length(input_filenames)),
  initial_phylogenies = rep(NA, length(input_filenames))
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
  if (class(initial_phylogenies) == "phylo") {
    initial_phylogenies <- c(initial_phylogenies)
    testit::assert(are_initial_phylogenies(initial_phylogenies))
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

  # 5 mcmc
  if (!is_mcmc(mcmc)) {
    stop(
      "'mcmc' must be a valid mcmc object, ",
      "as returned by 'create_mcmc'"
    )
  }

  # 6 misc_options
  if (!is_misc_options(misc_options)) {
    stop(
      "'misc_options' must be a valid misc options object, ",
      "as returned by 'create_misc_options'"
    )
  }

  # 7 fixed_crown_ages
  if (!is.logical(fixed_crown_ages)) {
    stop("'fixed_crown_ages' must be one or more booleans")
  }

  # 8 initial_phylogenies
  if (!are_initial_phylogenies(initial_phylogenies)) {
    stop("initial_phylogenies must be a list of NAs and phylo objects")
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
  if (length(input_filenames) != length(fixed_crown_ages)) {
    stop("Must supply as much input_filenames as fixed crown ages")
  }
  if (length(input_filenames) != length(initial_phylogenies)) {
    stop("Must supply as much input_filenames as initial_phylogenies")
  }

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
  testit::assert(are_init_site_models(site_models))  # nolint internal function
  testit::assert(are_init_clock_models(clock_models))  # nolint internal function
  testit::assert(are_init_tree_priors(tree_priors))  # nolint internal function

  # More complex
  if (has_shared_rln_clock_models(clock_models)) {
    stop("Cannot have shared Relaxed Log-Normal clock models")
  }

  # Make a million show as 1000000 instead of 1e+06
  options(scipen = 20)

  text <- create_beast2_input_beast(
    input_filenames = input_filenames,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mcmc = mcmc,
    misc_options = misc_options,
    fixed_crown_ages = fixed_crown_ages,
    initial_phylogenies = initial_phylogenies
  )
  text[1] <- paste0(create_beast2_input_xml(), text[1]) # nolint internal function

  text
}
