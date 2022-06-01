#' Get the site models' names
#' @return the site model names
#' @seealso Use \link{create_site_models} to get all site models
#' @examples
#' check_empty_beautier_folder()
#'
#' get_site_model_names()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_site_model_names <- function() {
  c("JC69", "HKY", "TN93", "GTR")
}
