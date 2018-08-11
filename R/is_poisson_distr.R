#' Determine if the object is a valid
#' Poisson distribution
#' as created by \code{\link{create_poisson_distr}}
#' @param x an object, to be determined if it is a valid
#'   Poisson distribution
#' @return TRUE if x is a valid Poisson distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @noRd
is_poisson_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "poisson") return(FALSE)
  if (!"lambda" %in% names(x)) return(FALSE)
  TRUE
}
