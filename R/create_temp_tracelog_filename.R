#' Create a filename for a temporary tracelog file
#'
#' @seealso use \link{create_tracelog} to create a tracelog.
#' @export
create_temp_tracelog_filename <- function() {
  normalizePath(
    get_beautier_tempfilename(
      pattern = "tracelog_",
      fileext = ".log"
    ),
    mustWork = FALSE
  )
}
