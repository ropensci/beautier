#' Determine if x consists out of MRCA priors
#' @inheritParams default_params_doc
#' @return TRUE if x, or all elements of x, are MRCA priors.
#'   Returns FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
are_mrca_priors <- function(
  mrca_priors
) {
  if (is_one_na(mrca_priors)) return(TRUE)
  if (!is.list(mrca_priors)) return(FALSE)
  for (i in seq_along(mrca_priors)) {
    mrca_prior <- mrca_priors[[i]]
    tryCatch(
      check_mrca_prior(mrca_prior),
      error = function(e) return(FALSE) # nolint indeed ignore e
    )
  }
  TRUE
}
