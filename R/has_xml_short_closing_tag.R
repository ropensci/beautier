#' Is an XML closing tag with short closing text in
#' one of the lines of the text?
#' @param lines lines of an XML text
#' @return TRUE if there is an XML tag that also closes present in the lines
#'   of text, FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' has_xml_short_closing_tag("<my_tag id=1/>")
#' # FALSE
#' has_xml_short_closing_tag("<my_tag id=1>text</my_tag>")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
has_xml_short_closing_tag <- function(
  lines
) {
  for (line in rev(lines)) {
    match <- stringr::str_trim(
      stringr::str_extract(
        string = line,
        pattern = "/>" # nolint this is no absolute path
      )
    )
    if (!is_one_na(match)) return(TRUE)
  }
  FALSE
}
