#' Get the full paths of files in the 'inst/extdata' folder
#' @param filenames the files' names, without the path
#' @return the filenames' full paths
#' @author Richel J.C. Bilderbeek
#' @seealso for one file, use \code{\link{get_beautier_path}}
#' @examples
#'   testit::assert(
#'     length(
#'       get_beautier_paths(
#'         c("test_output_0.fas", "anthus_aco.fas", "anthus_nd2.fas")
#'       )
#'      ) == 3
#'    )
#' @export
get_beautier_paths <- function(filenames) {

  for (i in seq_along(filenames)) {
    filenames[i] <- get_beautier_path(filenames[i]) # nolint internal function
  }

  filenames
}