#' Unindents text
#' @inheritParams default_params_doc
#' @return unindented lines of text
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
unindent <- function(
  text
) {
  stringr::str_sub(
    text,
    1 + min(nchar(stringr::str_match(text, "^([:space:]*)")[, 1]))
  )
}
