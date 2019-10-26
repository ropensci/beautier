#' Create a \code{tracelog} object
#' @inheritParams create_tracelog
#' @export
create_test_tracelog <- function(
  filename = tempfile(),
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
