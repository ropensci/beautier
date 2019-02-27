#' Get the XML opening tag
#' @inheritParams default_params_doc
#' @param text text to be determined to be valid
#' @return the openin tag if found, else NA
#' @examples
#'   testit::assert(
#'     beautier:::get_xml_opening_tag("<my_tag text=something/>")
#'     == "my_tag"
#'   )
#'   testit::assert(is_one_na(beautier:::get_xml_opening_tag("no_xml")))
#' @author Richèl J.C. Bilderbeek
#' @noRd
get_xml_opening_tag <- function(text) {
  first_line <- stringr::str_trim(text[1])

  # Get string like '<tag '
  xml_start <- stringr::str_extract(
    string = first_line,
    pattern = "<[A-Za-z_]* "
  )
  # Remove first '<' and last ' '
  stringr::str_sub(xml_start, start = 2L, end = -2L)
}
