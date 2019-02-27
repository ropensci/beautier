#' Extract everything between first loggers and last loggers line
#' @param lines lines of text
#' @return lines of text from the first to and including the last operators line
#' @author Richèl J.C. Bilderbeek
#' @noRd
extract_xml_loggers_from_lines <- function( # nolint beautier function
  lines
) {
  first_line <- find_first_regex_line(lines, "<logger id=\"") # nolint beautier function
  testit::assert(!is_one_na(first_line)) # nolint beautier function
  last_line <- find_last_regex_line(lines, "</logger>") # nolint beautier function
  lines[first_line:last_line]
}
