#' Find the line number of the first section's opening tag
#' @param lines the lines of an XML text
#' @param section the name of the XML section
#' @return the line number's index (which is 1 for the first line) if the
#'   opening tag is found, else NA
#' @author Richèl J.C. Bilderbeek
#' @noRd
find_first_xml_opening_tag_line <- function( # nolint beautier functions may be long
  lines,
  section
) {
  assertive::assert_is_a_string(section)
  beautier::find_first_regex_line(lines, paste0("<", section, ".*>"))
}
