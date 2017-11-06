#' Determine if the object is a valid mcmc object
#' @param x an object, to be determined if it is a valid mcmc object
#' @return TRUE if x is a valid mcmc object, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_mcmc <- function(
  x
) {
  if (!"chain_length" %in% names(x)) {
    return(FALSE)
  }
  if (get_mcmc_chain_length(x) < 10000) {
    return(FALSE)
  }
  TRUE
}
