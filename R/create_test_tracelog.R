#' Create a \code{tracelog} object, as used for testing
#' @inheritParams create_tracelog
#' @return a \code{tracelog} object
#' @examples
#' create_test_tracelog()
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_tracelog <- function(
  filename = create_temp_tracelog_filename(),
  log_every = 1000,
  mode = "autodetect",
  sanitise_headers = TRUE,
  sort = "smart"
) {
  create_tracelog(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitise_headers = sanitise_headers,
    sort = sort
  )
}
