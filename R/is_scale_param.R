#' Determine if the object is a valid
#' scale parameter
#' @param x an object, to be determined if it is a valid
#'   scale parameter
#' @return TRUE if x is a valid scale parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_scaleparam <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "scale")
}
