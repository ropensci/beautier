#' Create a \code{tracelog} object
#' @param filename name of the file to store the posterior traces
#' @param log_every number of MCMC states between the logging of that state
#' phylogenies to. By default, this is \code{$(trace).traces}
#' @param mode mode how to log.
#' Valid values are the ones returned by \link{get_log_modes}
#' @param sanitise_headers set to \link{TRUE} to sanitize the headers of the
#' log file
#' @param sort how to sort the log.
#' Valid values are the ones returned by \link{get_log_sorts}
#' @export
create_tracelog <- function(
  filename = "test_output_0.log",
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
