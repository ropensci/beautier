#' Determine if the object is a valid
#' 'rate AT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AT' parameter
#' @return TRUE if x is a valid 'rate AT' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_at_param}} creates a 'rate AT' parameter
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_at_param <- create_rate_at_param()
#'   testit::assert(beautier:::is_rate_at_param(rate_at_param))
is_rate_at_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_at"
}
