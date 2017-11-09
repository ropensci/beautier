#' Determine if the object is a valid
#' 1/x distribution
#' @param x an object, to be determined if it is a valid
#'   1/x distribution
#' @return TRUE if x is a valid 1/x distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distribution}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @export
is_one_div_x_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "one_div_x")
}
