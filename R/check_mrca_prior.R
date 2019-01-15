#' Check if the MRCA prior is a valid MRCA prior.
#"
#' Calls \code{stop} if the MRCA prior is invalid.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_mrca_prior} to create a valid MRCA prior
#' @examples
#'  testthat::expect_silent(check_mrca_prior(create_mrca_prior()))
#'
#'  # Must stop on non-MRCA priors
#'  testthat::expect_error(check_mrca_prior(mrca_prior = "nonsense"))
#'  testthat::expect_error(check_mrca_prior(mrca_prior = NULL))
#'  testthat::expect_error(check_mrca_prior(mrca_prior = NA))
#' @author Richel J.C. Bilderbeek
#' @export
check_mrca_prior <- function(mrca_prior) {
  if (is_mrca_prior(mrca_prior)) { # nolint beautier function
    return()
  }
  stop(
    "'mrca_prior' must be a valid MRCA prior.\n",
    "Actual value: ", mrca_prior
  )
}
