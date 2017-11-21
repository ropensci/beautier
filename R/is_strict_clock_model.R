#' Determine if the object is a valid strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param x an object, to be determined if it is a valid strict clock model
#' @return TRUE if x is a valid strict clock model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'   strict_clock_model <- create_strict_clock_model()
#'   testit::assert(is_strict_clock_model(strict_clock_model))
#'
#'   # rln: Relaxed Log-Normal
#'   rln_clock_model <- create_rln_clock_model()
#'   testit::assert(!is_strict_clock_model(rln_clock_model))
#' @export
is_strict_clock_model <- function(
  x
) {
  if(!"name" %in% names(x)) return(FALSE)
  if (x$name != "strict") return(FALSE)
  if(!"clock_rate_parameter" %in% names(x)) return(FALSE)
  TRUE
}
