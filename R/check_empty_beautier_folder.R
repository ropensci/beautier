#' Internal function
#'
#' Internal function to verify that, if there are `beautier`
#' temporary files created, these are also cleaned up,
#' as by CRAN policy.
#'
#' If the `beautier` folder does not exist, this function
#' does nothing.
#' If there are folder and/or files in the `beautier` folder,
#' an error is given.
#' @seealso use \link{remove_beautier_folder} to remove the default
#' `beautier` folder
#' @inheritParams default_params_doc
#' @return Nothing.
#' @examples
#' remove_beautier_folder()
#'
#' check_empty_beautier_folder()
#'
#' remove_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_empty_beautier_folder <- function(
  beautier_folder = get_beautier_folder()
) {
  beautier_folder <- normalizePath(beautier_folder, mustWork = FALSE)
  if (!dir.exists(beautier_folder)) return(invisible(beautier_folder))
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
  filenames <- normalizePath(
    list.files(
      beautier_folder, full.names = TRUE, recursive = TRUE
    )
  )
  if (length(filenames) != 0) {
    stop(
      "Files found in beautier folder. \n",
      "beautier_folder: ", beautier_folder, " \n",
      "length(list.files(beautier_folder))): ", length(filenames), " \n",
      "head(list.files(beautier_folder)): ",
      paste(utils::head(filenames), collapse = ",")
    )
  }
  if (dir.exists(beautier_folder)) {
    stop("'beautier' folder found at ", beautier_folder)
  }
  invisible(beautier_folder)
}
