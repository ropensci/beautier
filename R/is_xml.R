#' Checks if the text is a valid XML node, that is,
#' it has a opening and matching closing tag
#' @inheritParams default_params_doc
#' @param text text to be determined to be valid
#' @author Richel J.C. Bilderbeek
is_xml <- function(text) {
  if (length(text) == 1 && is.na(text)) return(FALSE)
  if (!has_xml_opening_tag(text)) return(FALSE)
  if (has_xml_short_closing_tag(text)) return(TRUE)
  opening_tag <- get_xml_opening_tag(text)
  closing_tag <- get_xml_closing_tag(text)
  if (opening_tag != closing_tag) return(FALSE)
  TRUE
}
