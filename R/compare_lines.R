#' Internal function
#'
#' Internal debug function to compare the actually created
#' lines to expected lines using any diff tool
#' @param lines the created lines
#' @param expected the expected/goal/target lines
#' @param section the XML section. Leave at NA to compare all lines
#' @param created_lines_filename name of the file where the (section of
#'   the) created lines are stored
#' @param expected_lines_filename name of the file where the (section of
#'   the) expected lines are stored
#' @return nothing. Instead, two files are created, with the
#'   names \code{created_lines_filename}
#'   and \code{expected_lines_filename} that contain the
#'   section under investigation, so that a diff tool
#'   can compare these
#' @author Richèl J.C. Bilderbeek
#' @export
compare_lines <- function(
  lines,
  expected,
  section = NA,
  created_lines_filename = "created.xml",
  expected_lines_filename = "expected.xml"
) {
  if (!beautier::is_one_na(section)) {
    lines <- beautier::extract_xml_section_from_lines(
      lines = lines, section = section)
    expected <- beautier::extract_xml_section_from_lines(
      lines = expected, section = section)
  }
  dir.create(
    dirname(created_lines_filename),
    showWarnings = FALSE,
    recursive = TRUE
  )
  dir.create(
    dirname(expected_lines_filename),
    showWarnings = FALSE,
    recursive = TRUE
  )
  writeLines(text = lines, created_lines_filename)
  writeLines(text = expected, expected_lines_filename)
}
