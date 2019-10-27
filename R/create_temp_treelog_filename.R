#' Create a filename for a temporary treelog file
#'
#' @seealso use \link{create_treelog} to create a treelog.
#' @export
create_temp_treelog_filename <- function() {
  tempfile(
    pattern = "treelog_",
    tmpdir = rappdirs::user_cache_dir(),
    fileext = ".trees"
  )
}
