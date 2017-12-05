#' Get the full path of a file in the 'inst/extdata' folder
#' @param filename the file's name, without the path
#' @return the filename's full path
#' @author Richel J.C. Bilderbeek
get_path <- function(filename) {

  full <- system.file("extdata", filename, package = "beautier")
  if (!file.exists(full)) {
    stop("'filename' must be the name of a file in 'inst/extdata'")
  }
  full
}
