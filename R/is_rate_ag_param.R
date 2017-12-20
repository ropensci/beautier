#' Determine if the object is a valid
#' 'rate AG' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AG' parameter
#' @return TRUE if x is a valid 'rate AG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ag_param}} creates a 'rate AG' parameter
#' @author Richel J.C. Bilderbeek
#' @examples
#'   rate_ag_param <- create_rate_ag_param()
is_rate_ag_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  x$name == "rate_ag"
}
