#' Determine if the object is a valid misc_options
#' @param x an object, to be determined if it is a misc_options
#' @return TRUE if the object is a valid misc_options, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'   misc_options <- create_misc_options()
#'   testit::assert(beautier:::is_misc_options(misc_options))
is_misc_options <- function(
  x
) {
  if (!"capitalize_first_char_id" %in% names(x)) return(FALSE)
  if (!"nucleotides_uppercase" %in% names(x)) return(FALSE)
  TRUE
}
