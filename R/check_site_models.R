#' Check if the object is a list of one or more site models.
#'
#' Will \link{stop} if the object is not a list of one or more site models.
#' @param site_models the object to be checked if it is a list of one
#'   or more valid site models
#' @return nothing.
#'   Will \link{stop} if the object is not a list of one or more site models.
#' @seealso Use \link{create_site_model} to create a valid site model
#' @examples
#' check_empty_beautier_folder()
#'
#' check_site_models(create_jc69_site_model())
#' check_site_models(list(create_jc69_site_model()))
#' check_site_models(
#'   list(create_jc69_site_model(), create_gtr_site_model())
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_site_models <- function(site_models) {

  if (is_site_model(site_models)) {
    site_models <- list(site_models)
  }
  if (!are_site_models(site_models)) {
    stop(
      "'site_models' must be a list of one or more valid site models. ",
      "Actual value(s): ", site_models
    )
  }

}
