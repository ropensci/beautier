#' Create a BEAST2 XML input text
#' @param input_fasta_filenames One or more fasta filenames
#' @param site_models one or more site models,
#'   as returned by \code{\link{create_site_model}}
#' @param mcmc one mcmc object,
#'   as returned by \code{\link{create_mcmc}}
#' @param tree_priors one or more tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param clock_models one or more clock models,
#'   as returned by \code{\link{create_clock_model}}
#' @param misc_options one misc_options object,
#'   as returned by \code{\link{create_misc_options}}
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param initial_phylogenies one or more MCMC chain's initial phylogenies.
#'   Each one set to NA will result in BEAST2 using a random phylogeny. Else
#'   the phylogeny is assumed to be of class ape::phylo.
#' @examples
#'   # Create a BEAST2 input file's text from the example FASTA file
#'   xml <- create_beast2_input(
#'     input_fasta_filenames = get_input_fasta_filename()
#'   )
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input <- function(
  input_fasta_filenames,
  site_models = create_jc69_site_models(n = length(input_fasta_filenames)),
  clock_models = create_strict_clock_models(ids = get_ids(input_fasta_filenames)),
  tree_priors = create_yule_tree_priors(n = length(input_fasta_filenames)),
  mcmc = create_mcmc(),
  misc_options = create_misc_options(),
  fixed_crown_age = FALSE,
  initial_phylogenies = rep(NA, length(input_fasta_filenames))
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
    testit::assert(class(initial_phylogenies) == "multiPhylo")
  }
  # Check input
  if (!beautier::files_exist(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }
  if (!are_site_models(site_models)) {
    stop("invalid site_models")
  }
  if (!are_clock_models(clock_models)) {
    stop("invalid clock_models")
  }
  if (!are_tree_priors(tree_priors)) {
    stop("tree_priors must be valid, as returned by 'create_tree_priors'")
  }
  if (!is_mcmc(mcmc)) {
    stop("mcmc must be a valid mcmc object, as returned by 'create_mcmc'")
  }
  if (!is.logical(fixed_crown_age)) {
    stop("fixed_crown_age must be either TRUE or FALSE")
  }
  if (length(input_fasta_filenames) != length(site_models)) {
    stop("Must supply as much input_fasta_filenames as site_models")
  }
  if (length(input_fasta_filenames) != length(clock_models)) {
    stop("Must supply as much input_fasta_filenames as clock_models")
  }
  if (length(input_fasta_filenames) != length(tree_priors)) {
    stop("Must supply as much input_fasta_filenames as tree priors")
  }
  if (length(input_fasta_filenames) != length(initial_phylogenies)) {
    stop("Must supply as much input_fasta_filenames as initial_phylogenies")
  }
  if (class(initial_phylogenies) != "multiPhylo" &&
      !is.na(initial_phylogenies)) {
    stop("initial_phylogenies must be either NA, ",
      "or of type 'phylo' or 'multiPhylo'")
  }

  clock_models <- initialize_clock_models(
    clock_models,
    distr_id = 0,
    param_id = 0
  )  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  tree_priors <- initialize_tree_priors( # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
    tree_priors,
    distr_id = 100,
    param_id = 200
  )
  testit::assert(are_initialized_clock_models(clock_models))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  testit::assert(are_initialized_tree_priors(tree_priors))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  # Make a million show as 1000000 instead of 1e+06
  options(scipen = 20)

  text <- create_beast2_input_beast(
      input_fasta_filenames = input_fasta_filenames,
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      mcmc = mcmc,
      misc_options = misc_options,
      fixed_crown_age = fixed_crown_age,
      initial_phylogenies = initial_phylogenies
  )
  text[1] <- paste0(create_beast2_input_xml(), text[1]) # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  text
}
