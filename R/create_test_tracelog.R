#' Create a \code{tracelog} object
#' @inheritParams create_tracelog
#' @export
create_test_tracelog <- function(
  filename = create_temp_tracelog_filename(),
  log_every = 1000,
  mode = "autodetect",
  sanitise_headers = TRUE,
  sort = "smart"
) {
  beautier::create_tracelog(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitise_headers = sanitise_headers,
    sort = sort
  )
}
