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
  utils::write.csv(lines, "~/created.csv")
  utils::write.csv(expected, "~/expected.csv")
  for (i in 1:min(length(expected), length(lines))) {
    testthat::expect_equal(
      expected[i], lines[i]
    )
    print(paste0(i, " / ", length(expected)))
  }
}
