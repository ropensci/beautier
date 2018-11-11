#' Determine if x is an initialized MRCA prior
#' @param x the object to check if it is an
#'   initialized MRCA prior
#' @return TRUE if x is an initialized MRCA prior
#' @author Richel J.C. Bilderbeek
#' @noRd
is_init_mrca_prior <- function(
  x
) {
  if (!is_mrca_prior(x)) return(FALSE) # nolint internal function
  # Just an NA
  if (length(x) == 1 && is.na(x)) return(TRUE)
  if (is.na(x$name)) return(FALSE)
  if (is.na(x$clock_prior_distr_id)) return(FALSE)
  if (is_distr(x$mrca_distr) &&  # nolint internal function
      !is_init_distr(x$mrca_distr) # nolint internal function
  ) return(FALSE)
  TRUE
}
