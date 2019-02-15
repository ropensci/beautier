#' Check if the MCMC is a valid MCMC object.
#'
#' Calls \code{stop} if the MCMC is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_mcmc} to create a valid MCMC
#' @examples
#'  testthat::expect_silent(check_mcmc(create_mcmc()))
#'
#'  # Must stop on non-MCMCs
#'  testthat::expect_error(check_mcmc(mcmc = "nonsense"))
#'  testthat::expect_error(check_mcmc(mcmc = NULL))
#'  testthat::expect_error(check_mcmc(mcmc = NA))
#' @author Richèl J.C. Bilderbeek
#' @export
check_mcmc <- function(mcmc) {
  if (is_mcmc(mcmc)) { # nolint beautier function
    return()
  }
  stop(
    "'mcmc' must be a valid MCMC.\n",
    "Actual value: ", mcmc
  )
}
