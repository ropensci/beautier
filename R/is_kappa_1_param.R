#' Determine if the object is a valid kappa 1 parameter
#' @param x an object, to be determined if it is a valid
#'   kappa 1 parameter
#' @return TRUE if x is a valid kappa 1 parameter,
#'   FALSE otherwise
#' @seealso kappa 1 parameters are returned by
#'   \code{\link{create_kappa_1_param}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   kappa_1_param <- create_kappa_1_param()
#'   testit::assert(beautier:::is_kappa_1_param(kappa_1_param))
is_kappa_1_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "kappa_1"
}
