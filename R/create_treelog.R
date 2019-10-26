#' Create a \code{treelog} object
#' @inheritParams default_params_doc
#' @param filename name of the file to store the posterior trees
#' @export
create_treelog <- function(
  filename = "$(tree).trees",
  log_every = 1000,
  mode = "tree",
  sanitise_headers = FALSE,
  sort = "none"
) {
  treelog <- list(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitise_headers = sanitise_headers,
    sort = sort
  )
  beautier::check_treelog(treelog)
  treelog
}
