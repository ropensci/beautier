#' Is an XML closing tag with short closing text at the end of the text?
#' @param lines lines of an XML text
#' @param section if NA, this function returns TRUE if there is any
#'   XML opening tag. If \code{section} is set to a certain word,
#'   this function returns TRUE if that tag matches \code{section}
#' @examples
#'   testit::assert(has_xml_short_closing_tag("<my_tag id=1/>"))
#'   testit::assert(!has_xml_short_closing_tag("<my_tag id=1>text</my_tag>"))
#' @author Richel J.C. Bilderbeek
has_xml_short_closing_tag <- function(
  lines
) {
  for (line in rev(lines)) {
    match <- stringr::str_trim(
      stringr::str_extract(
        string = line,
        pattern = "/>"
      )
    )
    if (!is.na(match)) return(TRUE)
  }
  FALSE
}
