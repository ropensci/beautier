#' Find duplicate RealParameter IDs
#' @param text the XML as text
#' @return a vector of duplicate IDs, will be empty if all IDs are unique
#' @seealso to see if all IDs are unqiue, use \code{\link{has_unique_ids}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   line_1 <- "<parameter id=\"RealParameter.1\" ...</parameter>"
#'   line_2 <- "<parameter id=\"RealParameter.2\" ...</parameter>"
#'   testit::assert(
#'     beautier:::get_duplicate_ids(c(line_1, line_2) == c("1", "2")))
#'   testit::assert(
#'     beautier:::get_duplicate_ids(c(line_1, line_1) == c("1")))
#'   testit::assert(
#'     beautier:::get_duplicate_ids(c(line_2, line_2) == c("2")))
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
  matches[duplicated(matches)]
}
