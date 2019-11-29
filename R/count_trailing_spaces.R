#' Count the number of spaces before the first character
#' @param line line of text
#' @return the number of spaces before the first character
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_equal(count_trailing_spaces("x"), 0)
#' expect_equal(count_trailing_spaces(" y"), 1)
#' expect_equal(count_trailing_spaces("  <"), 2)
#' expect_equal(count_trailing_spaces(""), 0)
#' expect_equal(count_trailing_spaces(" "), 1)
#' expect_equal(count_trailing_spaces("  "), 2)
#' @export
count_trailing_spaces <- function(
  line
) {
  for (i in 1:nchar(line)) {
    char <- substring(line, i, i)
    if (char != " ") return(i - 1)
  }
  nchar(line)
}
