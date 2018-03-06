#' Get the full path of a file in the 'inst/extdata' folder
#' @param filename the file's name, without the path
#' @return the filename's full path
#' @author Richel J.C. Bilderbeek
#' @seealso for more files, use \code{\link{get_beautier_paths}}
#' @examples
#'   testit::assert(is.character(get_beautier_path("test_output_0.fas")))
#'   testit::assert(is.character(get_beautier_path("anthus_aco.fas")))
#'   testit::assert(is.character(get_beautier_path("anthus_nd2.fas")))
#' @export
get_beautier_path <- function(filename) {

  full <- system.file("extdata", filename, package = "beautier")
  if (!file.exists(full)) {
    stop("'filename' must be the name of a file in 'inst/extdata'")
  }
  full
}
