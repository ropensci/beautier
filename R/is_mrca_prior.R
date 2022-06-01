#' Determine of the object is an empty (\code{NA}) or valid MRCA prior.
#'
#' @inheritParams default_params_doc
#' @return TRUE if \code{x} is an MRCA prior, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_mrca_prior(create_mrca_prior())
#' # Also 'NA' is a valid MRCA prior,
#' # denoting that there no MRCA priors
#' is_mrca_prior(NA)
#'
#' # FALSE
#' is_mrca_prior(NULL)
#' is_mrca_prior("nonsense")
#'
#' check_empty_beautier_folder()
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
