#' Extract everything between first operators and last operators line
#' @param lines lines of text
#' @return lines of text from the first to and including the last operators line
#' @author Richel J.C. Bilderbeek
extract_xml_operators_from_lines <- function( # nolint internal function
  lines
) {
  first_line <- find_first_regex_line(lines, "<operator id=\"") # nolint internal function
  if (is.na(first_line)) {
    return("")
  }
  last_line <- find_last_regex_line(lines, "(<operator id=\")|(</operator>)") # nolint internal function
  lines[first_line:last_line]
}