#' Create a filename for a temporary tracelog file
#'
#' @seealso use \link{create_tracelog} to create a tracelog.
#' @export
create_temp_tracelog_filename <- function() {
  normalizePath(
    tempfile(
      pattern = "tracelog_",
      tmpdir = rappdirs::user_cache_dir(),
      fileext = ".log"
    ),
    mustWork = FALSE
  )
}
