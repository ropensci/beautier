#' Get the path to the \link{beautier} temporary files folder
#'
#' Get the path to the \link{beautier} temporary files folder
#' @return the path to the \link{beautier} temporary files folder
#' @examples
#' check_empty_beautier_folder()
#'
#' get_beautier_folder()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_beautier_folder <- function() {
  normalizePath(
    rappdirs::user_cache_dir(appname = "beautier"),
    mustWork = FALSE
  )
}
