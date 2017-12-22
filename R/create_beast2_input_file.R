#' Create a BEAST2 input file
#' @inheritParams default_params_doc
#' @examples
#'   # The file created by beautier, a BEAST2 input file
#'   output_xml_filename <- "example_bd.xml"
#'
#'   # Birth-Death tree prior, crown age is estimated
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_fasta_filename(),
#'     output_xml_filename = output_xml_filename
#'   )
#'   testthat::expect_true(file.exists(output_xml_filename))
#'
#'   # The file created by beautier, a BEAST2 input file
#'   output_xml_filename_fixed <- "example_bd_fixed.xml"
#'
#'   # Birth-Death tree prior, crown age is fixed at 15 time units
#'   create_beast2_input_file(
#'     input_fasta_filenames = get_fasta_filename(),
#'     output_xml_filename = output_xml_filename_fixed,
#'     fixed_crown_ages = TRUE,
#'     initial_phylogenies = beautier::fasta_to_phylo(
#'       fasta_filename = get_fasta_filename(),
#'       crown_age = 15)
#'   )
#'   testthat::expect_true(file.exists(output_xml_filename_fixed))
#' @author Richel J.C. Bilderbeek
#' @seealso See \code{\link{create_site_model}} for examples with
#'   different site models. See \code{\link{create_clock_model}} for examples
#'   with clock models. See \code{\link{create_tree_prior}} for examples with
#'   different tree priors. See \code{\link{create_mcmc}} for examples with
#'   a different MCMC setup. See \code{\link{fasta_to_phylo}} for examples with
#'   a fixed crown age
#' @export
create_beast2_input_file <- function(
  input_fasta_filenames,
  output_xml_filename,
  site_models = create_jc69_site_models(ids = get_ids(input_fasta_filenames)),
  clock_models = create_strict_clock_models(
    ids = get_ids(input_fasta_filenames)),
  tree_priors = create_yule_tree_priors(ids = get_ids(input_fasta_filenames)),
  mcmc = create_mcmc(),
  fixed_crown_ages = rep(FALSE, length(input_fasta_filenames)),
  initial_phylogenies = rep(NA, length(input_fasta_filenames))
) {
  # Error handling done by create_beast2_input
  text <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mcmc = mcmc,
    fixed_crown_ages = fixed_crown_ages,
    initial_phylogenies = initial_phylogenies
  )

  # Write to file
  my_file <- file(output_xml_filename)
  writeLines(text, my_file)
  close(my_file)
}
