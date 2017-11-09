#' Determine if the object is a valid
#' uniform distribution
#' as created by \code{\link{create_uniform_distr}}
#' @param x an object, to be determined if it is a valid
#'   uniform distribution
#' @return TRUE if x is a valid uniform distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distribution}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @export
is_uniform_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "uniform")
}
