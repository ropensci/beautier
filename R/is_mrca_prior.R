#' Determine of the object is an empty (\code{NA}) or valid MRCA prior.
#'
#' @inheritParams default_params_doc
#' @return TRUE if \code{x} is an MRCA prior, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_true(is_mrca_prior(create_mrca_prior()))
#' # Also 'NA' is a valid MRCA prior,
#' # denoting that there no MRCA priors
#' expect_true(is_mrca_prior(NA))
#'
#' expect_false(is_mrca_prior(NULL))
#' expect_false(is_mrca_prior("nonsense"))
#' @export
is_mrca_prior <- function(
  mrca_prior
) {
  if (beautier::is_one_na(mrca_prior)) return(TRUE)
  if (!is.list(mrca_prior)) return(FALSE)
  tryCatch({
      beautier::check_mrca_prior(mrca_prior)
      TRUE
    },
    error = function(e) return(FALSE) # nolint indeed ignores e
  )
}
