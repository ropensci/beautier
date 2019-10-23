#' Create a \code{screenlog} object
#' @param filename name of the file to store the posterior screens
#' @param log_every number of MCMC states between the logging of that state
#' phylogenies to. By default, this is \code{$(screen).screens}
#' @param mode mode how to log.
#' Valid values are the ones returned by \link{get_log_modes}
#' @param sanitize_headers set to \link{TRUE} to sanitize the headers of the
#' log file
#' @param sort how to sort the log.
#' Valid values are the ones returned by \link{get_log_sorts}
#' @export
create_screenlog <- function(
  filename = "",
  log_every = 1000,
  mode = "autodetect",
  sanitize_headers = FALSE,
  sort = "none"
) {
  screenlog <- list(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitize_headers = sanitize_headers,
    sort = sort
  )
  beautier::check_screenlog(screenlog)
  screenlog
}
