#' Check if the argument is one string
#' @param x the argument to be tested to be one string
#' @examples
#' library(testthat)
#' expect_true(is_one_string("hello"))
#' expect_true(is_one_string("hello world"))
#' expect_false(is_one_string(NULL))
#' expect_false(is_one_string(NA))
#' expect_false(is_one_string(TRUE))
#' expect_false(is_one_string(c()))
#' expect_false(is_one_string(c("hello", "world")))
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_string <- function(x) {
  if (length(x) != 1) return(FALSE)
  if (is.na(x)) return(FALSE)
  is.character(x)
}
