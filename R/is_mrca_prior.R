#' Determine of the object is an empty (\code{NA}) or valid MRCA prior.
#'
#' @inheritParams default_params_doc
#' @param x object to be determined if it is an MRCA prior,
#'   as created by \code{\link{create_mrca_prior}}
#' @return TRUE if \code{x} is an MRCA prior, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @noRd
is_mrca_prior <- function(
  x
) {
  if (is_one_na(x)) return(TRUE) # nolint internal function
  if (!"name" %in% names(x)) return(FALSE)
  if (!"alignment_id" %in% names(x)) return(FALSE)
  if (!"taxa_names" %in% names(x)) return(FALSE)
  if (!"is_monophyletic" %in% names(x)) return(FALSE)
  if (!"mrca_distr" %in% names(x)) return(FALSE)
  if (!"clock_prior_distr_id" %in% names(x)) return(FALSE)
  if (!is_distr(x$mrca_distr) && !is.na(x$mrca_distr)) return(FALSE) # nolint internal function
  TRUE
}
