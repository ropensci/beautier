#' Remove all lines that are only whitespace
#' @param lines character vector with text
#' @param trim FALSE if indentation must be preserved,
#'   TRUE will remove all surrounding whitespace
#' @return the lines with text
#' @author Richèl J.C. Bilderbeek
#' @noRd
remove_empty_lines <- function(lines, trim = FALSE) {
  trimmed <- stringr::str_trim(lines)
  if (trim == FALSE) {
    lines[stringr::str_length(trimmed) > 0]
  }
  else {
    trimmed[stringr::str_length(trimmed) > 0]
  }
}
