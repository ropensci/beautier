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
#' inference_model <- create_ns_inference_model()
#' @author Richèl J.C. Bilderbeek
#' @export
create_ns_inference_model <- function(
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  tree_prior = beautier::create_yule_tree_prior(),
  mcmc = beautier::create_ns_mcmc()
) {
  beautier::check_site_model(site_model)
  beautier::check_clock_model(clock_model)
  beautier::check_tree_prior(tree_prior)
  beautier::check_nested_sampling_mcmc(mcmc)
  beautier::create_inference_model(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mcmc = mcmc
  )
}
