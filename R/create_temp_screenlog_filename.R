#' Create a filename for a temporary screenlog file
#'
#' @seealso use \link{create_screenlog} to create a screenlog.
#' @export
create_temp_screenlog_filename <- function() {
  normalizePath(
    get_beautier_tempfilename(
      pattern = "screenlog_",
      fileext = ".csv"
    ),
    mustWork = FALSE
  )
}
