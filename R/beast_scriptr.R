#' Deprecated interface for \code{\link{create_beast2_input_file}},
#'   present for backwards-compatibility
#' @param input_fasta_filename One or more fasta filename
#' @param output_xml_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @param mcmc_chainlength Length of MCMC chain
#' @param tree_prior One or more tree prior names,
#'   must be a name in \code{\link{get_tree_prior_names}}
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param initial_phylogeny the MCMC chain its initial phylogeny. If
#'   this is set to NA, BEAST2 will use a random phylogeny. Else
#'   a phylogeny must be supplied of class ape::phylo.
#' @examples
#'   # Get the filename of an example FASTA file
#'   input_fasta_filename <- get_input_fasta_filename()
#'   testit::assert(file.exists(input_fasta_filename))
#'
#'   # The file created by beastscriptr, a BEAST2 input file
#'   output_xml_filename <- "example_bd.xml"
#'
#'   # Birth-Death tree prior, crown age is estimated
#'   beast_scriptr(
#'     input_fasta_filename = get_input_fasta_filename(),
#'     output_xml_filename = output_xml_filename,
#'     mcmc_chainlength = 10000000,
#'     tree_prior = "birth_death"
#'   )
#'   testit::assert(file.exists(output_xml_filename))
#'
#'   # The file created by beastscriptr, a BEAST2 input file
#'   output_xml_filename_fixed <- "example_bd_fixed.xml"
#'
#'   # Birth-Death tree prior, crown age is fixed at 15 time units
#'   beast_scriptr(
#'     input_fasta_filename = get_input_fasta_filename(),
#'     output_xml_filename = output_xml_filename_fixed,
#'     mcmc_chainlength = 10000000,
#'     tree_prior = "birth_death",
#'     fixed_crown_age = TRUE,
#'     initial_phylogeny = beastscriptr::fasta_to_phylo(
#'       input_fasta_filename, crown_age = 15)
#'   )
#'   testit::assert(file.exists(output_xml_filename_fixed))
#' @author Richel J.C. Bilderbeek
#' @export
beast_scriptr <- function(
  input_fasta_filename,
  output_xml_filename,
  mcmc_chainlength = 10000000,
  tree_prior = "yule",
  fixed_crown_age = FALSE,
  initial_phylogeny = NA
) {

  create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    site_models = create_site_model(name = "JC69"),
    tree_priors = create_tree_prior(name = tree_prior),
    mcmc_chainlength = mcmc_chainlength,
    output_xml_filename = output_xml_filename,
    fixed_crown_age = fixed_crown_age,
    initial_phylogeny = initial_phylogeny
  )
}
