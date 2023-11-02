#' Create a \code{screenlog} object, to be used in testing
#' @inheritParams create_screenlog
#' @return a \code{screenlog} object
#' @examples
#' create_test_screenlog()
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_screenlog <- function(
  filename = create_temp_screenlog_filename(),
  log_every = 1000,
  mode = "autodetect",
  sanitise_headers = FALSE,
  sort = "none"
) {
  create_screenlog(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitise_headers = sanitise_headers,
    sort = sort
  )
}
