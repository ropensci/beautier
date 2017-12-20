#' Determines if the environment is Travis CI
#' @return TRUE if run on Travis CI, FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_on_travis <- function() {
  Sys.getenv("TRAVIS") != ""
}
