#' Determine if the object is a valid
#' 1/x distribution,
#' as created by \code{\link{create_one_div_x_distr}}
#' @param x an object, to be determined if it is a valid
#'   1/x distribution
#' @return TRUE if x is a valid 1/x distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
is_one_div_x_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "one_div_x")
}
