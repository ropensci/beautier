#' Create a site model from name
#' @inheritParams default_params_doc
#' @return a site model
#' @seealso Use \link{create_site_model} to create a site model
#' @examples
#' check_empty_beautier_folder()
#'
#' site_model <- create_site_model_from_name(get_site_model_names()[1])
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_site_model_from_name <- function(site_model_name) {
  if (site_model_name == "JC69") {
    beautier::create_jc69_site_model()
  } else if (site_model_name == "HKY") {
    beautier::create_hky_site_model()
  } else if (site_model_name == "TN93") {
    beautier::create_tn93_site_model()
  } else {
    testit::assert(site_model_name == "GTR")
    beautier::create_gtr_site_model()
  }
}
