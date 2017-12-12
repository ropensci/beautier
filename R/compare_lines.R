#' Internal debug function to compare the actually created
#' lines to expected lines.
#' @param lines the created lines
#' @param expected the expected/goal/target lines
#' @param section the XML section. Leave at NA to compare all lines
compare_lines <- function(
  lines,
  expected,
  section = NA
) {
  if (!is.na(section)) {
    lines <- extract_xml_section_from_lines(
      lines = lines, section = section)
    expected <- extract_xml_section_from_lines(
      lines = expected, section = section)
  }
  writeLines(lines, "~/created.xml")
  writeLines(expected, "~/expected.xml")
}
