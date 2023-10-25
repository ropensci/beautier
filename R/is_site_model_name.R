#' Determines if the name is a valid site_model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid site_model name, FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_site_model_name("JC69")
#' is_site_model_name("HKY")
#' is_site_model_name("TN93")
#' is_site_model_name("GTR")
#' # FALSE
#' is_site_model_name("nonsense")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
is_site_model_name <- function(name) {
  name %in% get_site_model_names()
}
