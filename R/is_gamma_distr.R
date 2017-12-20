#' Determine if the object is a valid
#' gamma distribution,
#' as created by \code{\link{create_gamma_distr}}
#' @param x an object, to be determined if it is a valid
#'   gamma distribution
#' @return TRUE if x is a valid gamma distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
is_gamma_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "gamma") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!"beta" %in% names(x)) return(FALSE)
  TRUE
}
