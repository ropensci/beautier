#' Determine if the object is a valid mcmc object
#' @param x an object, to be determined if it is a valid mcmc object
#' @seealso Use \link{\code{create_mcmc}}
#' @return TRUE if x is a valid mcmc object, FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_mcmc <- function(
  x
) {
  if (!"chain_length" %in% names(x)) return(FALSE)
  if (x$chain_length <= 0) return(FALSE)
  TRUE
}
