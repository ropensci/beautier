#' Create a simple inference model with a short MCMC chain
#' @inheritParams default_params_doc
#' @return an inference model
#' @seealso Use \link{create_inference_model} to create the
#'   BEAST2 default inference model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' inference_model <- create_test_inference_model()
#'
#' beast2_input_file <- tempfile(fileext = ".xml")
#' create_beast2_input_file_from_model(
#'   get_fasta_filename(),
#'   beast2_input_file,
#'   inference_model = inference_model
#' )
#' expect_true(file.exists(beast2_input_file))
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
  beautier::create_inference_model(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_prior,
    mcmc = mcmc,
    beauti_options = beauti_options,
    tipdates_filename = tipdates_filename
  )
}
