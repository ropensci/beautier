#' Count the number of spaces before the first character
#' @param line line of text
#' @return the number of spaces before the first character
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # 0
#' count_trailing_spaces("x")
#' # 1
#' count_trailing_spaces(" y")
#' # 2
#' count_trailing_spaces("  <")
#' # 0
#' count_trailing_spaces("")
#' # 1
#' count_trailing_spaces(" ")
#' # 2
#' count_trailing_spaces("  ")
#'
#' check_empty_beautier_folder()
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
