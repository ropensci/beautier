#' Determine if x is an initialized parameter,
#'   as created by \code{\link{create_param}}
#' @param x the object to check if it is an
#'   initialized parameter
#' @return TRUE if x is an initialized parameter, FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_init_param <- function(
  x
) {
  if (!is_param(x)) {
    stop("'x' must be a parameter")
  }
  !is.na(x$id)
}
