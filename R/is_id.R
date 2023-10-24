#' Determine if the object is a valid ID
#' @param x an object, to be determined if it is a valid ID
#' @return TRUE if x is a valid ID, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @seealso to check multiple IDs, use \link{are_ids}
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_id("anthus_aco")
#' is_id(3)
#'
#' # FALSE
#' is_id(ape::rcoal(3))
#' is_id(NULL)
#' is_id(NA)
#'
#' check_empty_beautier_folder()
#' @export
is_id <- function(
  x
) {
  rlang::is_string(x) || rlang::is_integerish(x, n = 1)
}
