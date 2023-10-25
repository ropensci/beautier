#' Get the full paths of files in the \code{inst/extdata} folder
#' @param filenames the files' names, without the path
#' @seealso Use \link{get_beautier_path} to get the path of one file
#' @return the filenames' full paths
#' @author Richèl J.C. Bilderbeek
#' @seealso for one file, use \code{\link{get_beautier_path}}
#' @examples
#' check_empty_beautier_folder()
#'
#'  get_beautier_paths(
#'    c("test_output_0.fas", "anthus_aco.fas", "anthus_nd2.fas")
#'  )
#'
#' check_empty_beautier_folder()
#' @export
get_beautier_paths <- function(filenames) {

  for (i in seq_along(filenames)) {
    filenames[i] <- get_beautier_path(filenames[i])
  }

  filenames
}
