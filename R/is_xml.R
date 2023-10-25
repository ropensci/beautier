#' Checks if the text is a valid XML node, that is,
#' it has a opening and matching closing tag
#' @param text text to be determined to be valid
#' @return TRUE if the text is valid XML, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
is_xml <- function(text) {
  if (is_one_na(text)) return(FALSE)
  if (!has_xml_opening_tag(text)) return(FALSE)
  if (has_xml_short_closing_tag(text)) return(TRUE)
  opening_tag <- get_xml_opening_tag(text)
  closing_tag <- get_xml_closing_tag(text)
  if (is_one_na(closing_tag)) return(FALSE)
  check_true(!is_one_na(closing_tag))
  if (opening_tag != closing_tag) return(FALSE)
  TRUE
}
