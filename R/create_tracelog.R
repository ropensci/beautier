#' Create a \code{tracelog} object
#' @inheritParams default_params_doc
#' @param filename
#'   name of the file to store the posterior traces.
#'   Use \link{NA} to use the filename \code{[alignment_id].log},
#'   where \code{alignment_id} is obtained using \link{get_alignment_id}
#' @export
create_tracelog <- function(
  filename = NA,
  log_every = 1000,
  mode = "autodetect",
  sanitise_headers = TRUE,
  sort = "smart"
) {
  tracelog <- list(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitise_headers = sanitise_headers,
    sort = sort
  )
  beautier::check_tracelog(tracelog)
  tracelog
}
