#' Determines if the name is a valid site_model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid site_model name, FALSE otherwise
#' @examples
#' library(testthat)
#'
#' expect_true(is_site_model_name("JC69"))
#' expect_true(is_site_model_name("HKY"))
#' expect_true(is_site_model_name("TN93"))
#' expect_true(is_site_model_name("GTR"))
#' expect_false(is_site_model_name("nonsense"))
#' @author Richèl J.C. Bilderbeek
#' @export
is_site_model_name <- function(name) {
  name %in% beautier::get_site_model_names()
}
