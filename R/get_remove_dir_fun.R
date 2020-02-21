#' Get a function that, from a filename, returns the part
#' without the directory.
#'
#' Or: get a function that returns the local version of a filename.
#' Also, the function will return \link{NA} if the filename is \link{NA}
#' @seealso see \link{check_rename_fun}
#'   for an overview of file renaming functions
#' @author Richèl J.C. Bilderbeek
#' @export
get_remove_dir_fun <- function() {
  function(filename) {
    stopifnot(length(filename) == 1)
    if (is.na(filename)) return(NA)
    basename(filename)
  }
}
