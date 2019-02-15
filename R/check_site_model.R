#' Check if the site model is a valid site model
#'
#' Calls \code{stop} if the site models are invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_site_model} to create a valid site model
#' @examples
#'  testthat::expect_silent(check_site_model(create_jc69_site_model()))
#'  testthat::expect_silent(check_site_model(create_hky_site_model()))
#'  testthat::expect_silent(check_site_model(create_tn93_site_model()))
#'  testthat::expect_silent(check_site_model(create_gtr_site_model()))
#'
#'  # Can use list of one site model
#'  testthat::expect_silent(check_site_model(list(create_jc69_site_model())))
#'
#'  # List of two site models is not a/one site model
#'  testthat::expect_error(
#'    check_site_model(
#'      list(create_jc69_site_model(), create_jc69_site_model())
#'    )
#'  )
#'
#'  # Must stop on non-site models
#'  testthat::expect_error(check_site_model(site_model = "nonsense"))
#'  testthat::expect_error(check_site_model(site_model = NULL))
#'  testthat::expect_error(check_site_model(site_model = NA))
#' @author Richèl J.C. Bilderbeek
#' @export
check_site_model <- function(site_model) {

  if (is_site_model(site_model)) { # nolint beautier function
    return()
  }
  if (length(site_model) == 1 && is_site_model(site_model[[1]])) { # nolint beautier function
    return()
  }
  stop(
    "'site_model' must be a valid site model.\n",
    "Actual value: ", site_model
  )

}
