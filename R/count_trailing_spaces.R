#' Count the number of spaces before the first character
#' @param line line of text
#' @return the number of spaces before the first character
#' @author Richel J.C. Bilderbeek
count_trailing_spaces <- function(
  line
) {
  for (i in 1:nchar(line)) {
    char <- substring(line, i, i)
    if (char != " ") return(i - 1)
  }
  nchar(line)
}
