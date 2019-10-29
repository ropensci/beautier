#' Determine if two tracelogs are equal.
#'
#' Will \link{stop} if the arguments are not tracelogs.
#' @param tracelog_1 an tracelog, as created by \link{create_tracelog}
#' @param tracelog_2 an tracelog, as created by \link{create_tracelog}
#' @return TRUE if the two tracelogs are equal
#' @seealso Use \link{create_tracelog} to create an tracelog
#' @examples
#' library(testthat)
#'
#' tracelog_1 <- create_tracelog(log_every = 1000)
#' tracelog_2 <- create_tracelog(log_every = 314)
#' expect_true(are_equal_tracelogs(tracelog_1, tracelog_1))
#' expect_false(are_equal_tracelogs(tracelog_1, tracelog_2))
#' @author Richèl J.C. Bilderbeek
#' @export
are_equal_tracelogs <- function(
  tracelog_1, tracelog_2
) {
  beautier::check_tracelog(tracelog_1)
  beautier::check_tracelog(tracelog_2)
  # Can be both NA
  if (is.na(tracelog_1$filename)) {
    if (!is.na(tracelog_2$filename)) return(FALSE)
  } else {
    if (tracelog_1$filename != tracelog_2$filename) return(FALSE)
  }
  tracelog_1$log_every == tracelog_2$log_every &&
    tracelog_1$mode == tracelog_2$mode &&
    tracelog_1$sanitise_headers == tracelog_2$sanitise_headers &&
    tracelog_1$sort == tracelog_2$sort
}
