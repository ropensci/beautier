#' Determine if the object is a valid ID
#' @param x an object, to be determined if it is a valid ID
#' @return TRUE if x is a valid ID, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @seealso to check multiple IDs, use \link{are_ids}
#' @examples
#' library(testthat)
#'
#' expect_true(is_id("anthus_aco"))
#' expect_true(is_id(3))
#' expect_false(is_id(ape::rcoal(3)))
#' expect_false(is_id(NULL))
#' expect_false(is_id(NA))
#' @export
is_id <- function(
  x
) {
  assertive::is_a_string(x) || beautier::is_one_int(x)
}
