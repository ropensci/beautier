#' Extract everything between first loggers and last loggers line
#' @param lines lines of text
#' @return lines of text from the first to and including the last operators line
#' @author Richèl J.C. Bilderbeek
#' @export
extract_xml_loggers_from_lines <- function(
  lines
) {
  first_line <- find_first_regex_line(lines, "<logger id=\"")
  check_true(!is_one_na(first_line))
  last_line <- find_last_regex_line(lines, "</logger>")
  lines[first_line:last_line]
}
