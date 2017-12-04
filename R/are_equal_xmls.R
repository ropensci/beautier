#' Determine if XML files result in equal trees
#' @param filename_1 name of a first XML file
#' @param filename_2 name of a second XML file
#' @param section name of an XML section.
#'   Assumes that there is one line that starts with \code{<section}
#'   (excluding whitespace)
#'   and one line that is \code{</section>} (also excluding whitespace)
#' @return TRUE if the two sections of the XML files are equal,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
are_equal_xml_files <- function(
  filename_1,
  filename_2,
  section
) {
  if (!file.exists(filename_1)) {
    stop(
      "'filename_1' must be the name of a file that is present, ",
      "'", filename_1, "' could not be found"
    )
  }
  if (!file.exists(filename_2)) {
    stop(
      "'filename_2' must be the name of a file that is present, ",
      "'", filename_2, "' could not be found"
    )
  }
  are_equal_xml_lines(
    lines_1 = readLines(filename_1),
    lines_2 = readLines(filename_2),
    section = section
  )
}

#' Determine if XML lines result in equal trees
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @param section name of an XML section.
#'   Assumes that there is one line that starts with \code{<section}
#'   (excluding whitespace)
#'   and one line that is \code{</section>} (also excluding whitespace)
#' @return TRUE if the two sections of the XML files are equal,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
are_equal_xml_lines <- function(
  lines_1,
  lines_2,
  section
) {
  if (!has_xml_opening_tag(lines = lines_1, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_closing_tag(lines = lines_1, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_opening_tag(lines = lines_2, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_closing_tag(lines = lines_2, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  section_1 <- extract_xml_section_from_lines(lines = lines_1, section = section)
  section_2 <- extract_xml_section_from_lines(lines = lines_2, section = section)
  identical(section_1, section_2)
}
