#' Creates the clock models' XML for the tracelog section
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_beast2_input_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
#' @noRd
clock_models_to_xml_tracelog <- function(
  clock_models,
  mrca_priors = NA
) {
  testit::assert(are_clock_models(clock_models)) # nolint internal function

  clock_models <- get_unlinked_clock_models(clock_models) # nolint internal function

  text <- NULL
  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(is_clock_model(clock_model)) # nolint internal function
    text <- c(
      text,
      clock_model_to_xml_tracelog( # nolint internal function
        clock_model = clock_model,
        is_first = i == 1,
        mrca_priors = mrca_priors
      )
    )
  }
  text
}
