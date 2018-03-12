#' Creates the clock model's XML for the tracelog section
#' @inheritParams default_params_doc
#' @seealso all clock models' tracelog section is created
#'   by \code{\link{clock_model_to_xml_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
clock_model_to_xml_tracelog <- function(
  clock_model,
  is_first
) {
  testit::assert(is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(is_id(id))

  text <- NULL
  if (is_rln_clock_model(clock_model)) {
    if (is_first == FALSE) {
      text <- c(text, paste0("<log idref=\"ucldMean.c:", id, "\"/>"))
    }
    text <- c(text, paste0("<log idref=\"ucldStdev.c:", id, "\"/>"))
    text <- c(text, paste0("<log id=\"rate.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>")
    )
  } else {
    # Will fail on unimplemented clock models
    testit::assert(is_strict_clock_model(clock_model))

    if (is_first == FALSE) {
      text <- c(text, paste0("<log idref=\"clockRate.c:", id, "\"/>"))
    }
  }
  testit::assert(is.null(text) || is_xml(text)) # nolint internal function
  text
}
