#' Determine if x is an initialized parameter,
#'   as created by \link{create_param}
#' @param x the object to check if it is an
#'   initialized parameter
#' @return \link{TRUE} if x is an initialized parameter,
#' \link{FALSE} otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_param <- function(
  x
) {
  if (!beautier::is_param(x)) {
    stop("'x' must be a parameter")
  }
  !beautier::is_one_na(x$id)
}
