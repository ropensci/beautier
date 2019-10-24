#' Create a Bayesian phylogenetic inference model.
#'
#' Create a Bayesian phylogenetic inference model,
#' as can be done by BEAUti.
#' @inheritParams default_params_doc
#' @return an MCMC configuration
#' @seealso Use \link{create_inference_model} to create an inference model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   # Create an MCMC chain with 50 states
#'   inference_model <- create_inference_model(
#'     mcmc = create_mcmc(chain_length = 50000, store_every = 1000)
#'   )
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file_from_model(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     inference_model = inference_model
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @export
create_inference_model <- function(
  site_model = create_jc69_site_model(),
  clock_model = create_strict_clock_model(),
  tree_prior = create_yule_tree_prior(),
  mrca_prior = NA,
  mcmc = create_mcmc(),
  beauti_options = create_beauti_options(),
  tipdates_filename = NA
) {
  inference_model <- list(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_prior,
    mcmc = mcmc,
    beauti_options = beauti_options,
    tipdates_filename = tipdates_filename
  )
  beautier::check_inference_model(inference_model)
  inference_model
}
