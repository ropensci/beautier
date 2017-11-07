#' Determine if x consists out of IDs
#' @param text the text to indent
#' @param n_spaces the number of spaces to add before the text
#' @return the indented text
#' @author Richel J.C. Bilderbeek
#' @export
indent <- function(
  text,
  n_spaces
) {
  if (n_spaces < 0) {
    stop("n_spaces must be positive")
  }
  spaces <- paste(rep(" ", n_spaces), collapse = "")
  paste0(spaces, text)
}
