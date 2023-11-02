#' Check there are no files in the default \link{beautier} folder
#'
#' Check there are no files in the default \link{beautier} folder.
#' The goal is to make sure no temporary files are left undeleted.
#' Will \link{stop} if there are files in the \link{beautier} folder.
#'
#' @seealso use \link{remove_beautier_folder} to remove the default
#' `beautier` folder
#' @return No return value, called for side effects.
#' @examples
#' check_empty_beautier_folder()
#'
#' remove_beautier_folder()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
remove_beautier_folder <- function() {
  folder_name <- get_beautier_folder()
  if (dir.exists(folder_name)) {
    unlink(folder_name, recursive = TRUE)
  }
}
