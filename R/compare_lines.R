#' Internal debug function to compare the actually created
#' lines to expected lines using any diff tool
#' @param lines the created lines
#' @param expected the expected/goal/target lines
#' @param section the XML section. Leave at NA to compare all lines
#' @param created_lines_filename name of the file where the (section of
#'   the) created lines are stored
#' @param expected_lines_filename name of the file where the (section of
#'   the) expected lines are stored
compare_lines <- function(
  lines,
  expected,
  section = NA,
  created_lines_filename = "~/created.xml",
  expected_lines_filename = "~/expected.xml"
) {
  if (!is.na(section)) {
    lines <- extract_xml_section_from_lines(
      lines = lines, section = section)
    expected <- extract_xml_section_from_lines(
      lines = expected, section = section)
  }
  writeLines(text = lines, created_lines_filename)
  writeLines(text = expected, expected_lines_filename)
}
