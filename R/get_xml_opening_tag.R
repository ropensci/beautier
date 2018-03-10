#' Get the XML opening tag
#' @inheritParams default_params_doc
#' @param text text to be determined to be valid
#' @author Richel J.C. Bilderbeek
get_xml_opening_tag <- function(text) {
  first_line <- text[1]
  # Get string like '<tag '
  xml_start <- stringr::str_extract(
    string = first_line,
    pattern = "<[A-Za-z_]* "
  )
  # Remove first '<' and last ' '
  stringr::str_sub(xml_start, start = 2L, end = -2L)
}
