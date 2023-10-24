#' Create a filename for a temporary `tracelog` file
#'
#' @return a filename for a temporary `tracelog` file
#' @seealso use \link{create_tracelog} to create a tracelog.
#' @examples
#' check_empty_beautier_folder()
#'
#' create_temp_tracelog_filename()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
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
