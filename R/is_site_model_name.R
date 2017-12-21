#' Determines if the name is a valid site_model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid site_model name, FALSE otherwise
#' @examples
#'   testit::assert(beautier:::is_site_model_name("JC69"))
#'   testit::assert(beautier:::is_site_model_name("HKY"))
#'   testit::assert(beautier:::is_site_model_name("TN93"))
#'   testit::assert(beautier:::is_site_model_name("GTR"))
#' @author Richel J.C. Bilderbeek
is_site_model_name <- function(name) {
  name %in% get_site_model_names() # nolint internal function
}
