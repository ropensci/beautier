#' Determine if `x` is `TRUE`
#' @param x the object to be determined to be `TRUE`
#' @return Nothing. Will raise an exception if the value is not `TRUE`
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_true(TRUE)
#' @export
check_true <- function(x) {
  testthat::expect_equal(length(x), 1)
  testthat::expect_false(is.na(x))
  testthat::expect_true(x)
}

#' Determine if `x` is `FALSE`
#' @param x the object to be determined to be `FALSE`
#' @return Nothing. Will raise an exception if the value is not `FALSE`
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_false(FALSE)
#' @export
check_false <- function(x) {
  testthat::expect_equal(length(x), 1)
  testthat::expect_false(is.na(x))
  testthat::expect_false(x)
}
