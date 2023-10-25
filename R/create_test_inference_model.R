#' Create a testing inference model.
#'
#' Creates a simple inference model with a short MCMC chain,
#' to be used in testing.
#' @inheritParams default_params_doc
#' @return an inference model
#' @seealso
#' Use \link{create_inference_model} to create a
#' regular inference model.
#' Use \link{create_test_ns_inference_model} to create an inference model
#' to estimate the marginal likelihood with a short MCMC, to be
#' used in testing
#' @examples
#' if (is_on_ci()) {
#'   check_empty_beautier_folder()
#'
#'   inference_model <- create_test_inference_model()
#'
#'   beast2_input_file <- get_beautier_tempfilename()
#'   create_beast2_input_file_from_model(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     inference_model = inference_model
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#'   check_empty_beautier_folder()
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_inference_model <- function(
  site_model = create_jc69_site_model(),
  clock_model = create_strict_clock_model(),
  tree_prior = create_yule_tree_prior(),
  mrca_prior = NA,
  mcmc = create_test_mcmc(),
  beauti_options = create_beauti_options(),
  tipdates_filename = NA
) {
  create_inference_model(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_prior,
    mcmc = mcmc,
    beauti_options = beauti_options,
    tipdates_filename = tipdates_filename
  )
}
