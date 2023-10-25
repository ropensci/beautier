#' Create an inference model to measure the evidence of.
#'
#' Create an inference model to measure the evidence of.
#' To do so, the inference model is created as usual (see
#' \link{create_inference_model}), except
#' for using a Nested Sampling MCMC (see \link{create_ns_mcmc})
#' @inheritParams default_params_doc
#' @return an inference model
#' @seealso
#' Use \link{create_inference_model} to create a
#' regular  inference model.
#' Use \link{create_test_ns_inference_model} to create an inference model
#' to estimate the marginal likelihood with a short MCMC,
#' to be used in testing.
#' @examples
#' check_empty_beautier_folder()
#'
#' inference_model <- create_ns_inference_model()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_ns_inference_model <- function(
  site_model = create_jc69_site_model(),
  clock_model = create_strict_clock_model(),
  tree_prior = create_yule_tree_prior(),
  mcmc = create_ns_mcmc()
) {
  check_site_model(site_model)
  check_clock_model(clock_model)
  check_tree_prior(tree_prior)
  check_nested_sampling_mcmc(mcmc)
  create_inference_model(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mcmc = mcmc
  )
}
