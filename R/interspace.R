#' Puts spaces in between the lines
#' @param lines lines of text
#' @return interspaced lines of text
#' @author Richèl J.C. Bilderbeek
#' @export
interspace <- function(lines) {

  if (length(lines) == 0) return(lines)

  # number of spaces of non-indented line
  nsni <- beautier::count_trailing_spaces(lines[1])

  result <- NULL
  for (i in seq_along(lines)) {
    line <- lines[i]
    result <- c(result, line)
    # Do not interspace if
    # - this is the last line
    # - this line is indented
    # - the next line is indented
    if (i == length(lines)) next
    if (beautier::count_trailing_spaces(line) == nsni
      && beautier::count_trailing_spaces(lines[i + 1]) == nsni
    ) {
      result <- c(result, "")
    }
  }
  result
}
