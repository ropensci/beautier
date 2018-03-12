#' Checks if the text is a valid XML node, that is,
#' it has a opening and matching closing tag
#' @inheritParams default_params_doc
#' @param text text to be determined to be valid
#' @author Richel J.C. Bilderbeek
is_xml <- function(text) {
  if (is_one_na(text)) return(FALSE) # nolint internal function
  if (!has_xml_opening_tag(text)) return(FALSE) # nolint internal function
  if (has_xml_short_closing_tag(text)) return(TRUE) # nolint internal function
  opening_tag <- get_xml_opening_tag(text) # nolint internal function
  closing_tag <- get_xml_closing_tag(text) # nolint internal function
  if (is.na(closing_tag)) return(FALSE)
  testit::assert(!is.na(closing_tag))
  if (opening_tag != closing_tag) return(FALSE)
  TRUE
}
