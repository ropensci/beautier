#' Is an XML opening tag with value 'section' present amongst the lines of
#'   the text?
#' @param lines lines of an XML text
#' @param section the XML section
has_xml_opening_tag <- function(
  lines,
  section
) {
  if (!is.character(section)) {
    stop("'section' must be a word")
  }
  !is.na(
    find_first_xml_opening_tag_line(
      lines = lines,
      section = section
    )
  )
}
