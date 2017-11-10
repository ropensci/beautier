#' Determine if the XML text has unique distribution IDs and unique
#'   parameter IDs
#' @param text the XML as text
#' @return TRUE if all IDs are unique, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
has_unique_ids <- function(
  text
) {
  matches <- text
  for (i in seq_along(matches)) {
    matches[i] <- stringr::str_extract(str = text[i], pattern = "RealParameter\\.[[:digit:]]?")
  }
  matches <- matches[ !is.na(matches) ]
  length(matches) == length(unique(matches))
}
