#' Determine if the XML text has unique distribution IDs and unique
#'   parameter IDs
#' @param text the XML as text
#' @return TRUE if all IDs are unique, FALSE otherwise
#' @seealso to obtain the duplicate IDs, use \code{\link{get_duplicate_ids}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   line_1 <- "<parameter id=\"RealParameter.1\" ...</parameter>"
#'   line_2 <- "<parameter id=\"RealParameter.2\" ...</parameter>"
#'   testit::assert(beautier:::has_unique_ids(c(line_1, line_2)))
#'   testit::assert(!beautier:::has_unique_ids(c(line_1, line_1)))
has_unique_ids <- function(
  text
) {
  length(get_duplicate_ids(text)) == 0 # nolint internal function
}
