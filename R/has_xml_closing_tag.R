#' Is an XML closing tag with the value of \code{section}
#'   present amongst the lines of
#'   the text?
#' @param lines lines of the XML text
#' @param section the XML section
#' @return TRUE if there is an XML closing tag with the value of
#'   \code{section} present. FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
has_xml_closing_tag <- function(
  lines,
  section
) {
  if (!is.character(section)) {
    stop("'section' must be a word")
  }
  !is_one_na( # nolint beautier function
    find_last_xml_closing_tag_line(
      lines = lines,
      section = section
    )
  )
}
