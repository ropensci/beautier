#' Create a \code{tracelog} object
#' @param filename name of the file to store the \code{tracelog} to.
#' Defaults to \code{[alignment_name].log}
#' @export
create_tracelog <- function(
  filename = "tracelog.log"
) {
  tracelog <- list(
    filename = filename
  )
  beautier::check_tracelog(tracelog)
  tracelog
}
