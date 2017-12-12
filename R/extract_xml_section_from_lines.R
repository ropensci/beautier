#' Get the lines of an XML section, including the section tags
#' @param lines lines of the XML text
#' @param section the XML section name
#' @return the section's lines of XML text, including the tags
#' @author Richel J.C. Bilderbeek
extract_xml_section_from_lines <- function(
  lines,
  section
) {
  if (!is.character(section)) {
    stop("'section' must be a word")
  }
  if (section == "operators") {
    return(extract_xml_operators_from_lines(lines)) # nolint internal function
  }
  if (section == "loggers") {
    return(extract_xml_loggers_from_lines(lines)) # nolint internal function
  }
  if (!has_xml_opening_tag(lines = lines, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_closing_tag(lines = lines, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines', ",
      "'section' has value '", section, "'"
    )
  }
  from_index <- find_first_xml_opening_tag_line(
    lines = lines,
    section = section
  )
  to_index <- find_last_xml_closing_tag_line(
    lines = lines,
    section = section
  )
  testit::assert(!is.na(from_index))
  testit::assert(!is.na(to_index))
  lines[from_index:to_index]
}
