#' Get the site models' names
#' @return the site model names
#' @seealso Use \link{create_site_models} to get all site models
#' @examples
#'   # Check all names
#'   names <- get_site_model_names()
#'   testit::assert("JC69" %in% names)
#'   testit::assert("HKY" %in% names)
#'   testit::assert("TN93" %in% names)
#'   testit::assert("GTR" %in% names)
#' @author Richèl J.C. Bilderbeek
#' @export
get_site_model_names <- function() {
  c("JC69", "HKY", "TN93", "GTR")
}
