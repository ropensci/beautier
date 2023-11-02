#' Create a \code{treelog} object to be used in testing
#' @inheritParams create_treelog
#' @return a \code{treelog} object
#' @examples
#' create_test_treelog()
#' @author Richèl J.C. Bilderbeek
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
