#' Get the BEAUti name for a clock model
#' @inheritParams default_params_doc
#' @examples
#'   strict <- create_strict_clock_model()
#'   testit::assert(beautier:::get_clock_model_name(strict) == "StrictClock")
#'   rln <- create_rln_clock_model()
#'   testit::assert(beautier:::get_clock_model_name(rln) == "RelaxedClock")
#' @author Richel J.C. Bilderbeek
get_clock_model_name <- function(
  clock_model
) {
  if (is_strict_clock_model(clock_model)) {
    return("StrictClock")
  } else {
    # Will fail on unimplemented clock models
    testit::assert(is_rln_clock_model(clock_model))
    return("RelaxedClock")
  }
}
