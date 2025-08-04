#' Determine if `x` is one boolean
#' @param x the object to be determined to be one boolean
#' @return Nothing. Will raise an exception if the value is not one boolean
#' @note Created by modifying check_bool from import-standalone-type-check.
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_bool(TRUE)
#' check_bool(FALSE)
#' @export
check_bool <- function(x) {
  testthat::expect_equal(length(x), 1)
  testthat::expect_false(is.na(x))
  testthat::expect_true(x == TRUE || x == FALSE)
}
