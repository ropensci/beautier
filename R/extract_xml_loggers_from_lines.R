#' Extract everything between first loggers and last loggers line
#' @param lines lines of text
#' @return lines of text from the first to and including the last operators line
#' @author Richel J.C. Bilderbeek
extract_xml_loggers_from_lines <- function( # nolint internal function
  lines
) {
  first_line <- find_first_regex_line(lines, "<logger id=\"") # nolint internal function
  testit::assert(!is.na(first_line))
  last_line <- find_last_regex_line(lines, "</logger>") # nolint internal function
  lines[first_line:last_line]
}
