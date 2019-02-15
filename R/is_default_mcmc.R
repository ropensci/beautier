#' Determine if the MCMC is a default MCMC
#' @return TRUE if the MCMC is a default MCMC
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   # An MCMC created by 'create_mcmc' is default
#'   testthat::expect_true(beautier:::is_default_mcmc(
#'     create_mcmc()
#'   ))
#'
#'   # An MCMC created by 'create_mcmc_nested_sampling' is not
#'   testthat::expect_false(beautier:::is_default_mcmc(
#'     create_mcmc_nested_sampling()
#'   ))
#' @noRd
is_default_mcmc <- function(mcmc) {
  if (!is_mcmc(mcmc)) return(FALSE) # nolint beautier function
  length(names(mcmc)) == 2
}
