#' Determine if the XML text has unique distribution IDs and unique
#'   parameter IDs
#' @param text the XML as text
#' @return TRUE if all IDs are unique, FALSE otherwise
#' @seealso to obtain the duplicate IDs, use \code{\link{get_duplicate_ids}}
#' @author Richel J.C. Bilderbeek
#' @export
has_unique_ids <- function(
  text
) {
  length(beautier::get_duplicate_ids(text)) == 0
}
