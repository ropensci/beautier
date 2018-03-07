#' Create a BEAST2 input file
#' @inheritParams default_params_doc
#' @examples
#'   # The file created by beautier, a BEAST2 input file
#'   output_filename <- "create_beast2_input_file.xml"
#'
#'   # Birth-Death tree prior, crown age is estimated
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     output_filename
#'   )
#'   testthat::expect_true(file.exists(output_filename))
#'
#'   # The file created by beautier, a BEAST2 input file
#'   output_filename_fixed <- "create_beast2_input_file_fixed.xml"
#'
#'   # Birth-Death tree prior, crown age is fixed at 15 time units
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     output_filename_fixed,
#'     posterior_crown_age = 15
#'   )
#'   testthat::expect_true(file.exists(output_filename_fixed))
#' @author Richel J.C. Bilderbeek
#' @seealso See \code{\link{create_site_model}} for examples with
#'   different site models. See \code{\link{create_clock_model}} for examples
#'   with clock models. See \code{\link{create_tree_prior}} for examples with
#'   different tree priors. See \code{\link{create_mcmc}} for examples with
#'   a different MCMC setup.
#' @export
create_beast2_input_file <- function(
  input_filenames,
  output_filename,
  site_models = create_jc69_site_models(ids = get_ids(input_filenames)),
  clock_models = create_strict_clock_models(
    ids = get_ids(input_filenames)),
  tree_priors = create_yule_tree_priors(ids = get_ids(input_filenames)),
  mrca_priors = NA,
  mcmc = create_mcmc(),
  posterior_crown_age = NA
) {
  # Error handling done by create_beast2_input
  text <- create_beast2_input(
    input_filenames = input_filenames,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mrca_priors = mrca_priors,
    mcmc = mcmc,
    posterior_crown_age = posterior_crown_age
  )

  # Write to file
  my_file <- file(output_filename)
  writeLines(text, my_file)
  close(my_file)
}
