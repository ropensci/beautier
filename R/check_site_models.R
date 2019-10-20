#' Check if the object is a list of one or more site models.
#'
#' Will \link{stop} if the object is not a list of one or more site models.
#' @param site_models the object to be checked if it is a list of one
#'   or more valid site models
#' @return nothing.
#'   Will \link{stop} if the object is not a list of one or more site models.
#' @seealso Use \link{create_site_model} to create a valid site model
#' @examples
#'   testthat::expect_silent(check_site_models(create_jc69_site_model()))
#'   testthat::expect_silent(check_site_models(list(create_jc69_site_model())))
#'   testthat::expect_silent(
#'     check_site_models(
#'       list(create_jc69_site_model(), create_gtr_site_model())
#'     )
#'   )
#'
#'   testthat::expect_error(check_site_models("nonsense"))
#'   testthat::expect_error(check_site_models(3.14))
#'   testthat::expect_error(check_site_models(42))
#'   testthat::expect_error(check_site_models(NA))
#'   testthat::expect_error(check_site_models(NULL))
#' @author Richèl J.C. Bilderbeek
#' @export
check_site_models <- function(site_models) {

  if (beautier::is_site_model(site_models)) {
    site_models <- list(site_models)
  }
  if (!beautier::are_site_models(site_models)) {
    stop(
      "'site_models' must be a list of one or more valid site models. ",
      "Actual value(s): ", site_models
    )
  }

}
