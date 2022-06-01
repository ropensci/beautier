#' Get the XML closing tag
#' @param text lines of XML to extract the XML closing tag from
#' @return the closing tag if found, else NA
#' @examples
#' check_empty_beautier_folder()
#'
#' # my_tag
#' get_xml_closing_tag("<my_tag text=something></my_tag>")
#'
#' # Will return NA
#' get_xml_closing_tag("<my_tag text=something/>")
#' get_xml_closing_tag("no_xml")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_xml_closing_tag <- function(text) {
  last_line <- stringr::str_trim(text[length(text)])

  # Get string like '<tag '
  xml_start <- stringr::str_extract(
    string = last_line,
    pattern = "</[A-Za-z_]*>"
  )
  # Remove first '</' and last '>'
  stringr::str_sub(xml_start, start = 3L, end = -2L)
}
