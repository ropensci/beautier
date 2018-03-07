#' Determine if x is an initialized mrca_prior objects
#' @param x the object to check if it is an
#'   initialized mrca_priors object
#' @return TRUE if x is an initialized mrca_prior object
#' @author Richel J.C. Bilderbeek
is_init_mrca_prior <- function(
  x
) {
  if (!is_mrca_prior(x)) return(FALSE)
  # Just an NA
  if (length(x) == 1 && is.na(x)) return(TRUE)
  if (is.na(x$name)) return(FALSE)
  if (is.na(x$clock_prior_distr_id)) return(FALSE)
  if (is_distr(x$mrca_distr) && !is_init_distr(x$mrca_distr)) return(FALSE)
  TRUE
}
