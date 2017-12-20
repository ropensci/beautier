#' Determine if the object is a valid
#' 'rate AC' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AC' parameter
#' @return TRUE if x is a valid 'rate AC' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ac_param}} creates a 'rate AC' parameter
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_ac_param <- create_rate_ac_param()
#'   testit::assert(beautier:::is_rate_ac_param(rate_ac_param))
is_rate_ac_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_ac"
}
