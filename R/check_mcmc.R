#' Check if the MCMC is a valid MCMC object.
#'
#' Calls \code{stop} if the MCMC is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_mcmc} to create a valid MCMC
#' @examples
#' library(testthat)
#'
#' expect_silent(check_mcmc(create_mcmc()))
#'
#'  # Must stop on non-MCMCs
#' expect_error(check_mcmc(mcmc = "nonsense"))
#' expect_error(check_mcmc(mcmc = NULL))
#' expect_error(check_mcmc(mcmc = NA))
#' @author Richèl J.C. Bilderbeek
#' @export
check_mcmc <- function(mcmc) {

  check_mcmc_list_element_names(mcmc)

  if (is_mcmc(mcmc)) { # nolint beautier function
    return()
  }
  stop(
    "'mcmc' must be a valid MCMC.\n",
    "Actual value: ", mcmc
  )
}




#' Check if the MCMC has the lst elements of a valid MCMC object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_mcmc} to create a valid MCMC
#' @author Richèl J.C. Bilderbeek
#' @noRd
check_mcmc_list_element_names <- function(mcmc) {

  list_element_names <- c(
    "chain_length", "store_every"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(mcmc)) {
      stop(
        "'", arg_name, "' must be an element of an 'mcmc'. \n",
        "Tip: use 'create_mcmc'"
      )
    }
  }
}
