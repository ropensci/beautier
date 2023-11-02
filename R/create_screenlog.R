#' Create a \code{screenlog} object
#' @inheritParams default_params_doc
#' @param filename name of the file to store the posterior screens
#' phylogenies to. By default, this is \code{$(screen).screens}
#' @return a \code{screenlog} object
#' @examples
#' check_empty_beautier_folder()
#'
#' create_screenlog()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_screenlog <- function(
  filename = "",
  log_every = 1000,
  mode = "autodetect",
  sanitise_headers = FALSE,
  sort = "none"
) {
  screenlog <- list(
    filename = filename,
    log_every = log_every,
    mode = mode,
    sanitise_headers = sanitise_headers,
    sort = sort
  )
  check_screenlog(screenlog)
  screenlog
}
