#' Determine of the object is an empty (\code{NA}) or valid MRCA prior.
#'
#' @inheritParams default_params_doc
#' @return TRUE if \code{x} is an MRCA prior, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @noRd
is_mrca_prior <- function(
  mrca_prior
) {
  if (is_one_na(mrca_prior)) return(TRUE)
  if (!is.list(mrca_prior)) return(FALSE)
  tryCatch(
    {
      check_mrca_prior(mrca_prior)
      TRUE
    },
    error = function(e) return(FALSE) # nolint indeed ignores e
  )
}
