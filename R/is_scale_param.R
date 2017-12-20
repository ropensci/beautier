#' Determine if the object is a valid
#' scale parameter
#' @param x an object, to be determined if it is a valid
#'   scale parameter
#' @return TRUE if x is a valid scale parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_scale_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "scale")
}
