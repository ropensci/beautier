#' Get the lines of an XML section, including the section tags
#' @param lines lines of the XML text
#' @param section the XML section name
#' @return the section's lines of XML text, including the tags
#' @author Richèl J.C. Bilderbeek
#' @noRd
extract_xml_section_from_lines <- function(
  lines,
  section
) {
  assertive::assert_is_a_string(section)
  if (section == "operators") {
    return(extract_xml_operators_from_lines(lines)) # nolint beautier function
  }
  if (section == "loggers") {
    return(beautier::extract_xml_loggers_from_lines(lines))
  }
  if (!has_xml_opening_tag(lines = lines, section = section)) { # nolint beautier function
    stop(
      "Opening tag for 'section' could not be found in 'lines', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_closing_tag(lines = lines, section = section)) { # nolint beautier function
    stop(
      "Closing tag for 'section' could not be found in 'lines', ",
      "'section' has value '", section, "'"
    )
  }
  from_index <- find_first_xml_opening_tag_line( # nolint beautier function
    lines = lines,
    section = section
  )
  to_index <- find_last_xml_closing_tag_line( # nolint beautier function
    lines = lines,
    section = section
  )
  testit::assert(!beautier::is_one_na(from_index))
  testit::assert(!beautier::is_one_na(to_index))
  lines[from_index:to_index]
}
