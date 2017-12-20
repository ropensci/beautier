#' Determine if the object is a valid mean parameter
#' @param x an object, to be determined if it is a valid mean parameter,
#'   as created by \code{\link{create_mean_param}})
#' @return TRUE if x is a valid mean parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_mean_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "mean")
}
