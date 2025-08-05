#' Determine if XML lines result in equal trees
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @param section name of an XML section.
#'   Assumes that there is one line that starts with \code{<section}
#'   (excluding whitespace)
#'   and one line that is \code{</section>} (also excluding whitespace)
#' @return TRUE if the two sections of the XML files are equal,
#'   FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' are_equal_xml_lines(
#'   lines_1 = readLines(get_beautier_path("2_4.xml")),
#'   lines_2 = readLines(get_beautier_path("2_6_0.xml")),
#'   section = "taxonset"
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
are_equal_xml_lines <- function(
  lines_1,
  lines_2,
  section
) {
  if (!beautier::has_xml_opening_tag(lines = lines_1, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!beautier::has_xml_closing_tag(lines = lines_1, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!beautier::has_xml_opening_tag(lines = lines_2, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  if (!beautier::has_xml_closing_tag(lines = lines_2, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  section_1 <- beautier::extract_xml_section_from_lines(
    lines = lines_1,
    section = section
  )
  section_2 <- beautier::extract_xml_section_from_lines(
    lines = lines_2,
    section = section
  )
  identical(section_1, section_2)
}
