#' Determine if two MCMCs are equal.
#'
#' Will \link{stop} if the arguments are not MCMCs.
#' @param mcmc_1 an MCMC, as created by \link{create_mcmc}
#' @param mcmc_2 an MCMC, as created by \link{create_mcmc}
#' @return TRUE if the two MCMCs are equal
#' @seealso Use \link{create_mcmc} to create an MCMC
#' @examples
#'   mcmc_1 <- create_mcmc(chain_length = 1000)
#'   mcmc_2 <- create_mcmc(chain_length = 314)
#'   testthat::expect_true(are_equal_mcmcs(mcmc_1, mcmc_1))
#'   testthat::expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
#' @author Richèl J.C. Bilderbeek
#' @export
are_equal_mcmcs <- function(
  mcmc_1, mcmc_2
) {
  check_mcmc(mcmc_1) # nolint beautier function
  check_mcmc(mcmc_2) # nolint beautier function
  mcmc_1$chain_length == mcmc_2$chain_length &&
    mcmc_1$store_every == mcmc_2$store_every
}
