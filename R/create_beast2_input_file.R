#' Create a BEAST2 input file
#' @inheritParams create_beast2_input
#' @param output_xml_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @examples
#'   # Get the filename of an example FASTA file
#'   input_fasta_filename <- get_input_fasta_filename()
#'   testit::assert(file.exists(input_fasta_filename))
#'
#'   # The file created by beastscriptr, a BEAST2 input file
#'   output_xml_filename <- "example_bd.xml"
#'
#'   # Birth-Death tree prior, crown age is estimated
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_input_fasta_filename(),
#'     output_xml_filename = output_xml_filename
#'   )
#'   testit::assert(file.exists(output_xml_filename))
#'
#'   # The file created by beastscriptr, a BEAST2 input file
#'   output_xml_filename_fixed <- "example_bd_fixed.xml"
#'
#'   # Birth-Death tree prior, crown age is fixed at 15 time units
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_input_fasta_filename(),
#'     output_xml_filename = output_xml_filename_fixed,
#'     fixed_crown_age = TRUE,
#'     initial_phylogeny = beastscriptr::fasta_to_phylo(
#'       input_fasta_filename, crown_age = 15)
#'   )
#'   testit::assert(file.exists(output_xml_filename_fixed))
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_file <- function(
  input_fasta_filenames,
  output_xml_filename,
  site_models = create_site_model(name = "JC69"),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule"),
  mcmc_chainlength = 10000000,
  fixed_crown_age = FALSE,
  initial_phylogeny = NA
) {
  if (!file.exists(input_fasta_filenames)) {
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
  text <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mcmc_chainlength,
    fixed_crown_age = fixed_crown_age,
    initial_phylogeny = initial_phylogeny
  )

  # Write to file
  my_file <- file(output_xml_filename)
  writeLines(text, my_file)
  close(my_file)
}
