#' Create a \code{treelog} object
#' @inheritParams default_params_doc
#' @param filename name of the file to store the posterior trees
#' @return a `treelog`, as can be checked by \link{check_treelog}
#' @examples
#' check_empty_beautier_folder()
#'
#' create_treelog()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
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
  check_treelog(treelog)
  treelog
}
