#' Create a BEAST2 XML input text
#' @param input_fasta_filenames One or more fasta filenames
#' @param site_models one or more site models,
#'   as returned by \code{\link{create_site_model}}
#' @param mcmc_chainlength Length of MCMC chain
#' @param tree_priors On or more tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param clock_models On or more clock models,
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
#'   # Create a BEAST2 input file's text from th example FASTA file
#'   xml <- create_beast2_input(
#'     input_fasta_filenames = get_input_fasta_filename()
#'   )
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input <- function(
  input_fasta_filenames,
  site_models = create_site_model(name = "JC69"),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule"),
  mcmc_chainlength = 10000000,
  misc_options = create_misc_options(),
  fixed_crown_age = FALSE,
  initial_phylogenies = rep(NA, length(input_fasta_filenames))
) {
  if (!beastscriptr::files_exist(input_fasta_filenames)) {
    stop("input_fasta_filenames not found")
  }
  if (!is_site_model(site_models)) {
    stop("invalid site_model")
  }
  if (!is_clock_model(clock_models)) {
    stop("invalid clock_model")
  }
  if (!is_tree_prior(tree_priors)) {
    stop("invalid tree_prior")
  }
  if (mcmc_chainlength <= 0) {
    stop("mcmc_chainlength must be positive")
  }
  if (!is.logical(fixed_crown_age)) {
    stop("fixed_crown_age must be either TRUE or FALSE")
  }
  if (length(input_fasta_filenames) != length(initial_phylogenies)) {
    stop("Must supply as much input_fasta_filenames as initial_phylogenies")
  }


  # Make a million show as 1000000 instead of 1e+06
  options(scipen = 20)

  text <- create_beast2_input_beast(
      input_fasta_filenames = input_fasta_filenames,
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      mcmc_chainlength = mcmc_chainlength,
      misc_options = misc_options,
      fixed_crown_age = fixed_crown_age,
      initial_phylogenies = initial_phylogenies
  )
  text[1] <- paste0(beastscriptr::create_beast2_input_xml(), text[1])
  text
}
