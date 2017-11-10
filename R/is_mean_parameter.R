#' Determine if the object is a valid
#' mean parameter
#' @param x an object, to be determined if it is a valid
#'   mean parameter
#' @return TRUE if x is a valid mean parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_mean_parameter <- function(
  x
) {
  if (!beautier::is_parameter(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "mean")
}
