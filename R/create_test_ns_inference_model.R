#' Create an inference model to be tested by Nested Sampling
#' @inheritParams default_params_doc
#' @export
create_test_ns_inference_model <- function(
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  tree_prior = beautier::create_yule_tree_prior(),
  mcmc = beautier::create_test_ns_mcmc()
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
