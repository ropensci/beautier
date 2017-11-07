#' Determine if the object is a valid
#' laplace distribution
#' @param x an object, to be determined if it is a valid
#'   laplace distribution
#' @return TRUE if x is a valid laplace distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_laplace_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "laplace")
}
