#' Determine if x is an initialized parameter,
#'   as created by \code{\link{create_param}}
#' @param x the object to check if it is an
#'   initialized parameter
#' @return TRUE if x is an initialized parameter, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_init_param <- function(
  x
) {
  if (!beautier::is_param(x)) {
    stop("'x' must be a parameter")
  }
  !beautier::is_one_na(x$id)
}
