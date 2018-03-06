#' Determine if the object is a valid mcmc object
#' @param x an object, to be determined if it is a valid mcmc object
#' @seealso Use \code{\link{create_mcmc}}
#' @return TRUE if x is a valid mcmc object, FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_mcmc <- function(
  x
) {
  if (!"chain_length" %in% names(x)) return(FALSE)
  if (x$chain_length <= 0) return(FALSE)
  if (!"store_every" %in% names(x)) return(FALSE)
  if (!is.na(x$store_every) && x$store_every < -1) return(FALSE)
  if (!is.na(x$store_every) && x$store_every == 0) return(FALSE)
  if (!is.na(x$store_every) && x$store_every > x$chain_length) return(FALSE)
  TRUE
}
