#' Determine if the object is a valid
#' mu parameter
#' @param x an object, to be determined if it is a valid
#'   mu parameter
#' @return TRUE if x is a valid mu parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_mu_param}} creates a mu parameter
#' @author Richel J.C. Bilderbeek
#' @examples
#'   mu_param <- create_mu_param()
#'   testit::assert(beautier:::is_mu_param(mu_param))
is_mu_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "mu"
}
