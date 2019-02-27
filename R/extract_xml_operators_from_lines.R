#' Extract everything between first operators and last operators line
#' @param lines lines of text
#' @return lines of text from the first to and including the last operators line
#' @author Richèl J.C. Bilderbeek
#' @noRd
extract_xml_operators_from_lines <- function( # nolint beautier function
  lines
) {
  first_line <- find_first_regex_line(lines, "<operator id=\"") # nolint beautier function
  if (is_one_na(first_line)) { # nolint beautier function
    return("")
  }
  last_line <- find_last_regex_line(lines, "(<operator id=\")|(</operator>)") # nolint beautier function
  lines[first_line:last_line]
}
