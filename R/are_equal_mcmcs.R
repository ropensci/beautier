#' Determine if two MCMCs are equal.
#'
#' Will \link{stop} if the arguments are not MCMCs.
#' @param mcmc_1 an MCMC, as created by \code{\link{create_mcmc}}
#' @param mcmc_2 an MCMC, as created by \code{\link{create_mcmc}}
#' @return TRUE if the two MCMCs are equal
#' @seealso Use \code{\link{create_mcmc}} to create an MCMC
#' @examples
#' if (is_on_ci()) {
#'
#'   check_empty_beautier_folder()
#'
#'   mcmc_1 <- create_mcmc(chain_length = 1000)
#'   mcmc_2 <- create_mcmc(chain_length = 314)
#'   # TRUE
#'   are_equal_mcmcs(mcmc_1, mcmc_1)
#'   # FALSE
#'   are_equal_mcmcs(mcmc_1, mcmc_2)
#'
#'   check_empty_beautier_folder()
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
are_equal_mcmcs <- function( # nolint cannot lower cyclomatic complexity
  mcmc_1, mcmc_2
) {
  check_mcmc(mcmc_1)
  check_mcmc(mcmc_2)
  mcmc_1$chain_length == mcmc_2$chain_length &&
    mcmc_1$store_every == mcmc_2$store_every &&
    mcmc_1$pre_burnin == mcmc_2$pre_burnin &&
    mcmc_1$n_init_attempts == mcmc_2$n_init_attempts &&
    mcmc_1$sample_from_prior == mcmc_2$sample_from_prior &&
    are_equal_tracelogs(mcmc_1$tracelog, mcmc_2$tracelog) &&
    are_equal_screenlogs(mcmc_1$screenlog, mcmc_2$screenlog) &&
    are_equal_treelogs(mcmc_1$treelog, mcmc_2$treelog)
}
