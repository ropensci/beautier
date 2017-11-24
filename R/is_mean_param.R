#' Determine if the object is a valid
#' mean parameter
#' @param x an object, to be determined if it is a valid
#'   mean parameter
#' @return TRUE if x is a valid mean parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_meanparam <- function(
  x
) {
  if (!beautier::isparam(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "mean")
}
