#' Determine if the object is a valid strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param x an object, to be determined if it is a valid strict clock model
#' @return TRUE if x is a valid strict clock model, FALSE otherwise
#' @seealso \code{\link{create_clock_model}} shows an overview of
#'   functions to create a clock model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   strict_clock_model <- create_strict_clock_model()
#'
#'   # rln: Relaxed Log-Normal
#'   rln_clock_model <- create_rln_clock_model()
#'   testit::assert(!beautier:::is_strict_clock_model(rln_clock_model))
is_strict_clock_model <- function(
  x
) {
  if (!is_clock_model(x)) return(FALSE)
  if (x$name != "strict") return(FALSE)
  if (!"clock_rate_param" %in% names(x)) return(FALSE)
  if (!is_param(x$clock_rate_param)) return(FALSE)
  if (!"clock_rate_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$clock_rate_distr)) return(FALSE)
  TRUE
}
