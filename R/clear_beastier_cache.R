#' Clear the \link{beautier} cache
#'
#' Clear the \link{beautier} cache.
#' @inheritParams default_params_doc
#' @return Nothing.
#' @author Richèl J.C. Bilderbeek
#' @export
clear_beautier_cache <- function(
  beautier_folder = get_beautier_folder()
) {
  dirs <- list.dirs(beautier_folder, full.names = TRUE)
  unlink(dirs, recursive = TRUE)
  files <- list.files(beautier_folder, full.names = TRUE)
  file.remove(files)
}
