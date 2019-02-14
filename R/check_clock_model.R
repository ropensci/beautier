#' Check if the clock model is a valid clock model
#' Calls \code{stop} if the clock models are invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_clock_model} to create a valid clock model
#' @examples
#'  testthat::expect_silent(check_clock_model(create_strict_clock_model()))
#'  testthat::expect_silent(check_clock_model(create_rln_clock_model()))
#'
#'  # Can use list of one clock model
#'  testthat::expect_silent(
#'    check_clock_model(list(create_strict_clock_model()))
#'  )
#'
#'  # List of two clock models is not a/one clock model
#'  testthat::expect_error(
#'    check_clock_model(
#'      list(create_strict_clock_model(), create_rln_clock_model())
#'    )
#'  )
#'
#'  # Must stop on non-clock models
#'  testthat::expect_error(check_clock_model(clock_model = "nonsense"))
#'  testthat::expect_error(check_clock_model(clock_model = NULL))
#'  testthat::expect_error(check_clock_model(clock_model = NA))
#' @author Richel J.C. Bilderbeek
#' @export
check_clock_model <- function(clock_model) {

  if (is_clock_model(clock_model)) { # nolint beautier function
    return()
  }
  if (length(clock_model) == 1 && is_clock_model(clock_model[[1]])) { # nolint beautier function
    return()
  }
  stop(
    "'clock_model' must be a valid clock model.\n",
    "Actual value: ", clock_model
  )

}
