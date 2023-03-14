#' Internal function
#'
#' Creates a strict clock model's XML for the tracelog section
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @examples
#' check_empty_beautier_folder()
#'
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
strict_clock_model_to_xml_tracelog <- function( # nolint indeed a long internal function name
  inference_model
) {
  testthat::expect_true(
    beautier::is_strict_clock_model(inference_model$clock_model)
  )
  clock_model <- inference_model$clock_model
  id <- clock_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  if (clock_model$clock_rate_param$estimate) {
    text <- c(
      text,
      paste0("<log idref=\"clockRate.c:", id, "\"/>")
    )
  }
  text
}
