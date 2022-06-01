#' Determines if the argument is a whole number
#' @param x the object to be determined of if it is one integer
#' @param tolerance tolerance to rounding errors
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_one_int(314)
#' is_one_int(0)
#' is_one_int(-314)
#' # FALSE
#' is_one_int(3.14)
#' is_one_int(NULL)
#' is_one_int(NA)
#' is_one_int(Inf)
#' is_one_int("nonsense")
#' is_one_int(c())
#' is_one_int(c(1, 2))
#'
#' check_empty_beautier_folder()
#' @export
is_one_int <- function(x, tolerance = .Machine$double.eps^0.5) {
  if (length(x) != 1) return(FALSE)
  if (is.function(x)) return(FALSE)
  if (is.na(x)) return(FALSE)
  if (is.infinite(x)) return(FALSE)
  if (!is.numeric(x)) return(FALSE)
  abs(x - round(x)) < tolerance
}
