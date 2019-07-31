#' Determines if the argument is a double
#' @param x the object to be determined of if it is one double
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_true(is_one_double(314))
#' expect_true(is_one_double(0))
#' expect_true(is_one_double(-314))
#' expect_true(is_one_double(3.14))
#' expect_false(is_one_double(NULL))
#' expect_false(is_one_double(NA))
#' expect_false(is_one_double(Inf))
#' expect_false(is_one_double("nonsense"))
#' expect_false(is_one_double(is_one_double))
#' expect_false(is_one_double(c()))
#' expect_false(is_one_double(c(1, 2)))
#' @export
is_one_double <- function(x) {
  if (length(x) != 1) return(FALSE)
  if (is.function(x)) return(FALSE)
  if (is.na(x)) return(FALSE)
  if (is.infinite(x)) return(FALSE)
  is.numeric(x)
}
