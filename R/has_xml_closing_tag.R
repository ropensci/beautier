#' Is an XML closing tag with the value of \code{section}
#'   present among the lines of
#'   the text?
#' @param lines lines of the XML text
#' @param section the XML section
#' @return TRUE if there is an XML closing tag with the value of
#'   \code{section} present. FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
has_xml_closing_tag <- function(
  lines,
  section
) {
  assertive::assert_is_a_string(section)
  !beautier::is_one_na(
    beautier::find_last_xml_closing_tag_line(
      lines = lines,
      section = section
    )
  )
}
