##' Determine if `x` is one string
##' @param x the object to be determined to be one string
##' @param allow_empty boolean to indicate if an empty string
##' is allowed
##' @inheritParams default_params_doc
##' @return Nothing. Will raise an exception if the value is not one string
##' @author Richèl J.C. Bilderbeek
##' @examples
##' check_string("This is a string :-)")
##' @export
#check_string <- function(
#  x,
#  allow_na = FALSE,
#  allow_empty = TRUE
#) {
#  check_bool(allow_na)
#  check_bool(allow_empty)
#  if (length(x) != 1) {
#    stop(
#      "'x' must be a single string. \n",
#      "Actual length: ", length(x)
#    )
#  }
#  if (!allow_na) {
#    if (is_one_na(x)) {
#      stop(
#        "'x' must be a single string, not NA"
#      )
#    }
#  }
#  if (is.na(x)) return()
#  testthat::expect_true(is.character(x))
#  if (!allow_empty) {
#    testthat::expect_true(nchar(x) > 0)
#  }
#}
