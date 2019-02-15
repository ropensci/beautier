#' Get the BEAUti name for a clock model
#'
#' Will \link{stop} if the clock model is an invalid clock model
#' @inheritParams default_params_doc
#' @return name of the clock model
#' @examples
#'   strict <- create_strict_clock_model()
#'   testit::assert(beautier:::get_clock_model_name(strict) == "StrictClock")
#'   rln <- create_rln_clock_model()
#'   testit::assert(beautier:::get_clock_model_name(rln) == "RelaxedClock")
#' @author Richèl J.C. Bilderbeek
#' @noRd
get_clock_model_name <- function(
  clock_model
) {
  if (is_strict_clock_model(clock_model)) { # nolint beautier function
    return("StrictClock")
  } else {
    # Will fail on unimplemented clock models
    testit::assert(is_rln_clock_model(clock_model)) # nolint beautier function
    return("RelaxedClock")
  }
}
