#' Checks if the text is a valid XML node
#' @inheritParams default_params_doc
#' @param text text to be determined to be valid
#' @examples
#' @author Richel J.C. Bilderbeek
is_xml <- function(text) {
  !is.na(text)
}
