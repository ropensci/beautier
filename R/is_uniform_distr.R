#' Determine if the object is a valid
#' uniform distribution
#' as created by \code{\link{create_uniform_distr}}
#' @param x an object, to be determined if it is a valid
#'   uniform distribution
#' @return TRUE if x is a valid uniform distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
is_uniform_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "uniform")
}
