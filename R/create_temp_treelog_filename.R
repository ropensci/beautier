#' Create a filename for a temporary `treelog` file
#'
#' @return a filename for a temporary `treelog` file
#' @seealso use \link{create_treelog} to create a treelog.
#' @examples
#' check_empty_beautier_folder()
#'
#' create_temp_treelog_filename()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_temp_treelog_filename <- function() {
  normalizePath(
    get_beautier_tempfilename(
      pattern = "treelog_",
      fileext = ".trees"
    ),
    mustWork = FALSE
  )
}
