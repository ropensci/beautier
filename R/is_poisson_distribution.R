#' Determine if the object is a valid
#' poisson distribution
#' @param x an object, to be determined if it is a valid
#'   poisson distribution
#' @return TRUE if x is a valid poisson distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_poisson_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "poisson")
}
