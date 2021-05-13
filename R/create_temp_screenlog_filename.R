#' Create a filename for a temporary screenlog file
#'
#' @seealso use \link{create_screenlog} to create a screenlog.
#' @export
create_temp_screenlog_filename <- function() {
  normalizePath(
    tempfile(
      pattern = "screenlog_",
      tmpdir = rappdirs::user_cache_dir(),
      fileext = ".csv"
    ),
    mustWork = FALSE
  )
}
