#' Determine if the object is a valid
#' laplace distribution,
#' as created by \code{\link{create_laplace_distr}}
#' @param x an object, to be determined if it is a valid
#'   laplace distribution
#' @return TRUE if x is a valid laplace distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @export
is_laplace_distr <- function(
  x
) {
  if (!beautier::is_distr(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "laplace")
}
