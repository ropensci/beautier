#' Determine if the object is a valid ID
#' @param x an object, to be determined if it is a valid ID
#' @return TRUE if x is a valid ID, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_id <- function(
  x
) {
  is.character(x)
}
