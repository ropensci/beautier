#' Remove all lines that are only whitespace
#' @param lines character vector with text
#' @return the lines with text
#' @author Richel J.C. Bilderbeek
#' @noRd
remove_empty_lines <- function(lines) {
  trimmed <- stringr::str_trim(lines)
  lines[stringr::str_length(trimmed) > 0]
}
