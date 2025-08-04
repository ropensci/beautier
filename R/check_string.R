#' Determine if `x` is one string
#' @param x the object to be determined to be one string
#' @inheritParams default_params_doc
#' @return Nothing. Will raise an exception if the value is not one string
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_string("This is a string :-)")
#' @export
check_string <- function(
  x,
  allow_na = FALSE,
  allow_empty = FALSE
) {
  check_bool(allow_na)
  check_bool(allow_empty)
  testthat::expect_equal(length(x), 1)
  if (!allow_na) {
    testthat::expect_false(is.na(x))
  }
  if (is.na(x)) return()
  testthat::expect_true(is.character(x))
  if (!allow_empty) {
    testthat::expect_true(nchar(x) > 0)
  }
}
