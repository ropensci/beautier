#' Get the path to the \link{beautier} temporary files folder
#'
#' Get the path to the \link{beautier} temporary files folder
#' @return the path to the \link{beautier} temporary files folder
#' @examples
#' get_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_beautier_folder <- function() {
  rappdirs::user_cache_dir(appname = "beautier")
}
