#' Create a \code{treelog} object
#' @inheritParams create_treelog
#' @export
create_test_treelog <- function(
  filename = create_temp_treelog_filename(),
  log_every = 1000,
  mode = "tree",
  sanitise_headers = FALSE,
  sort = "none"
) {
  create_treelog(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitise_headers = sanitise_headers,
    sort = sort
  )
}
