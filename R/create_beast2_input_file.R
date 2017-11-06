#' Create a BEAST2 input file
#' @inheritParams create_beast2_input
#' @param output_xml_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @examples
#'   # The file created by beautier, a BEAST2 input file
#'   output_xml_filename <- "example_bd.xml"
#'
#'   # Birth-Death tree prior, crown age is estimated
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_input_fasta_filename(),
#'     output_xml_filename = output_xml_filename
#'   )
#'   testthat::expect_true(file.exists(output_xml_filename))
#'
#'   # The file created by beautier, a BEAST2 input file
#'   output_xml_filename_fixed <- "example_bd_fixed.xml"
#'
#'   # Birth-Death tree prior, crown age is fixed at 15 time units
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_input_fasta_filename(),
#'     output_xml_filename = output_xml_filename_fixed,
#'     fixed_crown_age = TRUE,
#'     initial_phylogenies = beautier::fasta_to_phylo(
#'       fasta_filename = get_input_fasta_filename(),
#'       crown_age = 15)
#'   )
#'   testthat::expect_true(file.exists(output_xml_filename_fixed))
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_file <- function(
  input_fasta_filenames,
  output_xml_filename,
  site_models = create_site_model(name = "JC69"),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule"),
  mcmc_chainlength = get_default_mcmc_chain_length(),
  fixed_crown_age = FALSE,
  initial_phylogenies = rep(NA, length(input_fasta_filenames))
) {
  if (!beautier::files_exist(input_fasta_filenames)) {
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
  if (class(initial_phylogenies) == "phylo") {
    initial_phylogenies <- c(initial_phylogenies)
    testit::assert(class(initial_phylogenies) == "multiPhylo")
  }
  if (length(input_fasta_filenames) != length(initial_phylogenies)) {
    stop("Must supply as much input_fasta_filenames as initial_phylogenies")
  }

  text <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mcmc_chainlength = mcmc_chainlength,
    fixed_crown_age = fixed_crown_age,
    initial_phylogenies = initial_phylogenies
  )

  # Write to file
  my_file <- file(output_xml_filename)
  writeLines(text, my_file)
  close(my_file)
}
