#' Determine if the MCMC is a default MCMC
#' @inheritParams default_params_doc
#' @return TRUE if the MCMC is a default MCMC
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#'  # An MCMC created by 'create_mcmc' is default
#' expect_true(is_default_mcmc(create_mcmc()))
#'
#' # An MCMC created by 'create_mcmc_nested_sampling' is not
#' expect_false(is_default_mcmc(create_mcmc_nested_sampling()))
#' @export
is_default_mcmc <- function(mcmc) {
  if (!is_mcmc(mcmc)) return(FALSE) # nolint beautier function
  length(names(mcmc)) == length(names(create_mcmc()))
}
