#' Create a filename for a temporary `screenlog` file
#'
#' @return a filename for a temporary `screenlog` file
#' @seealso use \link{create_screenlog} to create a screenlog.
#' @examples
#' check_empty_beautier_folder()
#'
#' create_temp_screenlog_filename()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_temp_screenlog_filename <- function() {
  normalizePath(
    beautier::get_beautier_tempfilename(
      pattern = "screenlog_",
      fileext = ".csv"
    ),
    mustWork = FALSE
  )
}
