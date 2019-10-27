#' Create a \code{screenlog} object
#' @inheritParams create_screenlog
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
