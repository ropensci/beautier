#' Get the XML closing tag
#' @inheritParams default_params_doc
#' @param text lines of XML to extract the XML closing tag from
#' @return the closing tag if found, else NA
#' @examples
#'   testit::assert(
#'     beautier:::get_xml_closing_tag("<my_tag text=something></my_tag>")
#'     == "my_tag"
#'   )
#'   testit::assert(
#'     is_one_na(
#'       beautier:::get_xml_closing_tag("<my_tag text=something/>")
#'     )
#'   )
#'   testit::assert(is_one_na(beautier:::get_xml_closing_tag("no_xml")))
#' @author Richèl J.C. Bilderbeek
#' @noRd
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
