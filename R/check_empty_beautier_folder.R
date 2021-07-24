#' Check there are no files in the default \link{beautier} folder
#'
#' Check there are no files in the default \link{beautier} folder.
#' The goal is to make sure no temporary files are left undeleted.
#' Will \link{stop} if there are files in the \link{beautier} folder
#' @inheritParams default_params_doc
#' @return Nothing.
#' @examples
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_empty_beautier_folder <- function(
  beautier_folder = get_beautier_folder()
) {
  dirs <- normalizePath(list.dirs(beautier_folder))
  dirs <- dirs[dirs != normalizePath(beautier_folder, mustWork = FALSE)]

  if (length(dirs) != 0) {
    stop(
      "Folders found in beautier folder. \n",
      "beautier_folder: ", beautier_folder, " \n",
      "length(list.dirs(beautier_folder)): ", length(dirs), " \n",
      "head(list.dirs(beautier_folder)): ",
        paste(utils::head(dirs), collapse = ",")
    )
  }
  filenames <- list.files(beautier_folder, full.names = TRUE, recursive = TRUE)
  if (length(filenames) != 0) {
    stop(
      "Files found in beautier folder. \n",
      "beautier_folder: ", beautier_folder, " \n",
      "length(list.files(beautier_folder))): ", length(filenames), " \n",
      "head(list.files(beautier_folder)): ",
        paste(utils::head(filenames), collapse = ",")
    )
  }
}
