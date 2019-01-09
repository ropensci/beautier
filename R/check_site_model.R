#' Check if the site model is a valid site model
#' Calls \code{stop} if the site models are invalid
#' @inheritParams default_params_doc
#' @param x object to be determined of, if it is a valid site model,
#'   as can be created by \link{create_site_model}
#' @return nothing
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
#'   check_site_model(
#'     list(create_jc69_site_model(), create_jc69_site_model())
#'   )
#'  )
#'
#'  # Must stop on non-site models
#'  testthat::expect_error(check_site_model(site_model = "nonsense"))
#'  testthat::expect_error(check_site_model(site_model = NULL))
#'  testthat::expect_error(check_site_model(site_model = NA))
#' @author Richel J.C. Bilderbeek
#' @export
check_site_model <- function(x) {

  if (is_site_model(x)) { # nolint internal function
    return()
  }
  if (length(x) == 1 && is_site_model(x[[1]])) { # nolint internal function
    return()
  }
  stop(
    "Object must be a valid site model, ",
    "as returned by 'create_site_model'.\n",
    "Actual value: ", x
  )

}
