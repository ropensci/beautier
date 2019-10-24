#' Checks if the text is a valid XML node, that is,
#' it has a opening and matching closing tag
#' @inheritParams default_params_doc
#' @param text text to be determined to be valid
#' @return TRUE if the text is valid XML, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
is_xml <- function(text) {
  if (beautier::is_one_na(text)) return(FALSE)
  if (!has_xml_opening_tag(text)) return(FALSE) # nolint beautier function
  if (has_xml_short_closing_tag(text)) return(TRUE) # nolint beautier function
  opening_tag <- get_xml_opening_tag(text) # nolint beautier function
  closing_tag <- get_xml_closing_tag(text) # nolint beautier function
  if (beautier::is_one_na(closing_tag)) return(FALSE)
  testit::assert(!beautier::is_one_na(closing_tag))
  if (opening_tag != closing_tag) return(FALSE)
  TRUE
}
