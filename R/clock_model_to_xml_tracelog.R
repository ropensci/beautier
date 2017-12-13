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
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (beautier::is_rln_clock_model(clock_model)) {
    text <- c(text, paste0("<log idref=\"ucldStdev.c:", id, "\"/>"))
    text <- c(text, paste0("<log id=\"rate.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>")
    )
  }
  if (is_first == FALSE) {
    text <- c(text, paste0("<log idref=\"clockRate.c:", id, "\"/>"))
  }
  text
}
