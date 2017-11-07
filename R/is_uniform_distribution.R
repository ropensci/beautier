#' Determine if the object is a valid
#' uniform distribution
#' @param x an object, to be determined if it is a valid
#'   uniform distribution
#' @return TRUE if x is a valid uniform distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_uniform_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "uniform")
}
