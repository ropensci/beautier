#' Get the filename's base without extension
#' @param filename A filename
#' @return That filename without its full path and extension
#' @examples
#'   testit::assert(get_file_base_sans_ext("/home/richel/test.txt") == "test")
#' @export
get_file_base_sans_ext <- function(filename) {
  return(basename(tools::file_path_sans_ext(filename)))
}
