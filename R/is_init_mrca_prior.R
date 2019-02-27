#' Determine if x is an initialized MRCA prior
#' @param x the object to check if it is an
#'   initialized MRCA prior
#' @return TRUE if x is an initialized MRCA prior
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_init_mrca_prior <- function(
  x
) {
  if (!is_mrca_prior(x)) return(FALSE) # nolint beautier function
  # Just an NA
  if (length(x) == 1 && is_one_na(x)) return(TRUE) # nolint beautier function
  if (is_one_na(x$name)) return(FALSE) # nolint beautier function
  if (is_one_na(x$clock_prior_distr_id)) return(FALSE) # nolint beautier function
  if (is_distr(x$mrca_distr) &&  # nolint beautier function
      !is_init_distr(x$mrca_distr) # nolint beautier function
  ) return(FALSE)
  TRUE
}
