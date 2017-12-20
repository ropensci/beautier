#' Creates the clock models' XML for the tracelog section
#' @inheritParams default_params_doc
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_beast2_input_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_tracelog <- function(
  clock_models
) {
  testit::assert(are_clock_models(clock_models))

  clock_models <- get_unlinked_clock_models(clock_models) # nolint internal function

  text <- NULL
  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(is_clock_model(clock_model))
    text <- c(text, clock_model_to_xml_tracelog(clock_model, i == 1)) # nolint internal function
  }
  text
}
