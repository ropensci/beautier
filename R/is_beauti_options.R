#' Determine if the object is a valid beauti_options
#' @param x an object, to be determined if it is a beauti_options
#' @return TRUE if the object is a valid beauti_options, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   beauti_options <- create_beauti_options()
#'   testit::assert(beautier:::is_beauti_options(beauti_options))
#' @noRd
is_beauti_options <- function(
  x
) {
  if (!"capitalize_first_char_id" %in% names(x)) return(FALSE)
  if (!"nucleotides_uppercase" %in% names(x)) return(FALSE)
  if (!"sequence_indent" %in% names(x)) return(FALSE)
  TRUE
}
