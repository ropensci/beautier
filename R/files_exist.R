#' Multi-file file.exists function
#' @param filenames one or more filename
#' @return TRUE if all files exists, FALSE otherwise
#' @author Richel J.C. Bilderbeek
files_exist <- function(filenames) {
  for (filename in filenames) {
    if (!file.exists(filename)) {
      return(FALSE)
    }
  }
  TRUE
}
