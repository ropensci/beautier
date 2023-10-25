#' Create an inference model to be tested by Nested Sampling
#' @inheritParams default_params_doc
#' @return an inference model
#' @seealso
#' Use \link{create_test_inference_model} to create a
#' regular inference model with a short MCMC, to be used in testing.
#' Use \link{create_ns_inference_model} to create an inference model
#' to estimate the marginal likelihood.
#' @examples
#' check_empty_beautier_folder()
#'
#' inference_model <- create_test_ns_inference_model()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_ns_inference_model <- function(
  site_model = create_jc69_site_model(),
  clock_model = create_strict_clock_model(),
  tree_prior = create_yule_tree_prior(),
  mcmc = create_test_ns_mcmc()
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
