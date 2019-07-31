#' Check if the argument is one boolean
#' @param x the argument to be tested to be boolean
#' @examples
#' library(testthat)
#' expect_true(is_one_bool(TRUE))
#' expect_true(is_one_bool(FALSE))
#' expect_false(is_one_bool(NULL))
#' expect_false(is_one_bool(NA))
#' expect_false(is_one_bool(c()))
#' expect_false(is_one_bool("nonsense"))
#' expect_false(is_one_bool(is_one_bool))
#' expect_false(is_one_bool(c(TRUE, FALSE)))
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_bool <- function(x) {
  assertive::is_if_condition(x)
}
