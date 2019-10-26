#' Extract everything between first loggers and last loggers line
#' @param lines lines of text
#' @return lines of text from the first to and including the last operators line
#' @author Richèl J.C. Bilderbeek
#' @export
extract_xml_loggers_from_lines <- function( # nolint beautier function
  lines
) {
  first_line <- beautier::find_first_regex_line(lines, "<logger id=\"")
  testit::assert(!beautier::is_one_na(first_line))
  last_line <- beautier::find_last_regex_line(lines, "</logger>")
  lines[first_line:last_line]
}
