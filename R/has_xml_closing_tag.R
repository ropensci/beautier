#' Is an XML opening tag with value 'section' present amongst the lines of
#'   the text?
#' @param lines lines of the XML text
#' @param section the XML section
has_xml_closing_tag <- function(
  lines,
  section
) {
  if (!is.character(section)) {
    stop("'section' must be a word")
  }
  !is.na(
    find_last_xml_closing_tag_line(
      lines = lines,
      section = section
    )
  )
}
