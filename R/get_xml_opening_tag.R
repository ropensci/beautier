#' Get the XML opening tag
#' @param text text to be determined to be valid
#' @return the opening tag if found, else NA
#' @examples
#' check_empty_beautier_folder()
#'
#' # my_tag
#' get_xml_opening_tag("<my_tag text=something/>")
#'
#' # NA when there is no opening tag
#' get_xml_opening_tag("no_xml")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
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
