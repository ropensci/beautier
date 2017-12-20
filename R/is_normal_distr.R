#' Determine if the object is a valid
#' normal distribution
#' as created by \code{\link{create_normal_distr}}
#' @param x an object, to be determined if it is a valid
#'   normal distribution
#' @return TRUE if x is a valid normal distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
is_normal_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "normal") return(FALSE)
  if (!"mean" %in% names(x)) return(FALSE)
  if (!"sigma" %in% names(x)) return(FALSE)
  TRUE
}
