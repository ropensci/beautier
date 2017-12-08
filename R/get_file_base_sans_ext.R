#' Get the filename's base without extension
#' @param filename A filename
#' @return That filename without its full path and extension
#' @examples
#'   testit::assert(
#'     beautier:::get_file_base_sans_ext("/home/richel/test.txt")
#'     == "test"
#'  )
get_file_base_sans_ext <- function(filename) {
  basename(tools::file_path_sans_ext(filename))
}
