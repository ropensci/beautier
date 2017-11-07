#' Determine if the object is a valid
#' exponential distribution
#' @param x an object, to be determined if it is a valid
#'   exponential distribution
#' @return TRUE if x is a valid exponential distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_exponential_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "exponential")
}
