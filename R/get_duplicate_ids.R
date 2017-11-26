#' Find duplicate RealParameter IDs
#' @param text the XML as text
#' @return a vector of duplicate IDs, will be empty if all IDs are unique
#' @seealso to see if all IDs are unqiue, use \code{\link{has_unique_ids}}
#' @author Richel J.C. Bilderbeek
#' @export
get_duplicate_ids <- function(
  text
) {
  if (!is.character(text)) {
    stop("'text' must be text")
  }
  matches <- text
  for (i in seq_along(matches)) {
    matches[i] <- stringr::str_extract(
      str = text[i],
      pattern = "RealParameter\\.[[:digit:]]+")
  }
  matches <- matches[!is.na(matches)]
  matches[ duplicated(matches) ]
}
