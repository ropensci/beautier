#' Get the site models' names
#' @return the site model names
#' @examples
#'   names <- beautier:::get_site_model_names()
#'   testit::assert("JC69" %in% names)
#'   testit::assert("HKY" %in% names)
#'   testit::assert("TN93" %in% names)
#'   testit::assert("GTR" %in% names)
#' @author Richel J.C. Bilderbeek
get_site_model_names <- function() {
  c("JC69", "HKY", "TN93", "GTR")
}
