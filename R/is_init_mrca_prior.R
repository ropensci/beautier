#' Determine if x is an initialized MRCA prior
#' @param x the object to check if it is an
#'   initialized MRCA prior
#' @return TRUE if x is an initialized MRCA prior
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_mrca_prior <- function(
  x
) {
  if (!is_mrca_prior(x)) return(FALSE)
  # Just an NA
  if (length(x) == 1 && is_one_na(x)) return(TRUE)
  if (is_one_na(x$name)) return(FALSE)
  if (is_one_na(x$clock_prior_distr_id)) return(FALSE)
  if (is_distr(x$mrca_distr) &&
      !is_init_distr(x$mrca_distr)
  ) return(FALSE)
  TRUE
}
