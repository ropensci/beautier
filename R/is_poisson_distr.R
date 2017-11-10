#' Determine if the object is a valid
#' poisson distribution
#' as created by \code{\link{create_poisson_distr}}
#' @param x an object, to be determined if it is a valid
#'   poisson distribution
#' @return TRUE if x is a valid poisson distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @export
is_poisson_distr <- function(
  x
) {
  if (!beautier::is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "poisson") return(FALSE)
  if (!"m" %in% names(x)) return(FALSE)
  if (!"s" %in% names(x)) return(FALSE)
  TRUE
}
