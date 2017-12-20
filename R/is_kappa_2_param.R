#' Determine if the object is a valid kappa 2 parameter
#' @param x an object, to be determined if it is a valid
#'   kappa 2 parameter
#' @return TRUE if x is a valid kappa_2 parameter,
#'   FALSE otherwise
#' @seealso kappa 2 parameters are returned by
#'   \code{\link{create_kappa_2_param}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   kappa_2_param <- create_kappa_2_param()
#'   testit::assert(beautier:::is_kappa_2_param(kappa_2_param))
is_kappa_2_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "kappa_2"
}
