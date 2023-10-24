#' Extract everything between first operators and last operators line
#' @param lines lines of text
#' @return lines of text from the first to and including the last operators line
#' @author Richèl J.C. Bilderbeek
#' @export
extract_xml_operators_from_lines <- function(# nolint indeed a long function name
  lines
) {
  first_line <- find_first_regex_line(lines, "<operator id=\"")
  if (is_one_na(first_line)) {
    return("")
  }
  last_line <- find_last_regex_line(
    lines,
    "(<operator id=\")|(</operator>)"
  )
  lines[first_line:last_line]
}
