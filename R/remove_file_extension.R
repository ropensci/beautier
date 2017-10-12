#' Remove a file extension
#' @param filename A filename
#' @return That filename without its extension
#' @export
remove_file_extension <- function(filename) {
  return(strsplit(filename, "\\.")[[1]][1])
  # Better version
  # return(tools::file_path_sans_ext(filename))
}

