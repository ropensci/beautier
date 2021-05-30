#' Unindents text
#' @param text one or more lines of text
#' @return unindented lines of text
#' @author Richèl J.C. Bilderbeek
#' @export
unindent <- function(
  text
) {
  if (length(text) == 0) return(text)
  stringr::str_sub(
    text,
    1 + min(nchar(stringr::str_match(text, "^([:space:]*)")[, 1]))
  )
}
