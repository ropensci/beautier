#' Create a filename for a temporary treelog file
#'
#' @seealso use \link{create_treelog} to create a treelog.
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
