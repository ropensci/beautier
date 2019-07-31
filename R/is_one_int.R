#' Determines if the argument is a whole number
#' @param x the object to be determined of if it is one integer
#' @param tolerance tolerance to rounding errors
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_true(is_one_int(314))
#' expect_true(is_one_int(0))
#' expect_true(is_one_int(-314))
#' expect_false(is_one_int(3.14))
#' expect_false(is_one_int(NULL))
#' expect_false(is_one_int(NA))
#' expect_false(is_one_int(Inf))
#' expect_false(is_one_int("nonsense"))
#' expect_false(is_one_int(c()))
#' expect_false(is_one_int(c(1, 2)))
#' @export
is_one_int <- function(x, tolerance = .Machine$double.eps^0.5) {
  if (length(x) != 1) return(FALSE)
  if (is.function(x)) return(FALSE)
  if (is.na(x)) return(FALSE)
  if (is.infinite(x)) return(FALSE)
  if (!is.numeric(x)) return(FALSE)
  abs(x - round(x)) < tolerance
}
