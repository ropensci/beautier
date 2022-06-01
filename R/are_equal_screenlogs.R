#' Determine if two screenlogs are equal.
#'
#' Will \link{stop} if the arguments are not screenlogs.
#' @param screenlog_1 an screenlog, as created by \link{create_screenlog}
#' @param screenlog_2 an screenlog, as created by \link{create_screenlog}
#' @return TRUE if the two screenlogs are equal
#' @seealso Use \link{create_screenlog} to create an screenlog
#' @examples
#' check_empty_beautier_folder()
#'
#' screenlog_1 <- create_screenlog(log_every = 1000)
#' screenlog_2 <- create_screenlog(log_every = 314)
#' # TRUE
#' are_equal_screenlogs(screenlog_1, screenlog_1)
#' # FALSE
#' are_equal_screenlogs(screenlog_1, screenlog_2)
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
are_equal_screenlogs <- function(
  screenlog_1, screenlog_2
) {
  beautier::check_screenlog(screenlog_1)
  beautier::check_screenlog(screenlog_2)
  # Can be both NA
  if (is.na(screenlog_1$filename)) {
    if (!is.na(screenlog_2$filename)) return(FALSE)
  } else {
    if (screenlog_1$filename != screenlog_2$filename) return(FALSE)
  }
  screenlog_1$log_every == screenlog_2$log_every &&
    screenlog_1$mode == screenlog_2$mode &&
    screenlog_1$sanitise_headers == screenlog_2$sanitise_headers &&
    screenlog_1$sort == screenlog_2$sort
}
