#' Get the full paths of files in the 'inst/extdata' folder
#' @param filenames the files' names, without the path
#' @return the filenames' full path
#' @author Richel J.C. Bilderbeek
get_paths <- function(filenames) {

  for (i in seq_along(filenames)) {
    filenames[i] <- get_path(filenames[i])
  }

  filenames
}
