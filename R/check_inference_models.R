#' Check if the \code{inference_model} is a valid BEAUti inference model.
#'
#' Calls \code{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_inference_model} to create a valid
#'   BEAST2 options object
#' @examples
#'  testthat::expect_silent(
#'    check_inference_models(
#'        list(create_inference_model()
#'      )
#'    )
#'  )
#'
#'  # Must stop on nonsense
#'  testthat::expect_error(check_inference_models("nonsense"))
#'  testthat::expect_error(check_inference_models(NULL))
#'  testthat::expect_error(check_inference_models(NA))
#' @author Richèl J.C. Bilderbeek
#' @export
check_inference_models <- function(
  inference_models
) {
  if (!is.list(inference_models)) {
    stop("'inference_models' must be a list")
  }
  for (i in seq_along(inference_models)) {
    # Stub with too simple error message
    check_inference_model(inference_models[[i]]) # nolint beautier function
  }
}
