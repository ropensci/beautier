#' Determine if the object is a valid ID
#' @param x an object, to be determined if it is a valid ID
#' @return TRUE if x is a valid ID, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @seealso to check multiple IDs, use \code{\link{are_ids}}
#' @noRd
is_id <- function(
  x
) {
  assertive::is_a_string(x) || is_one_int(x) # nolint beautier function
}
