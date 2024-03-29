#' Get the BEAUti name for a clock model
#'
#' Will \link{stop} if the clock model is an invalid clock model
#' @inheritParams default_params_doc
#' @return name of the clock model
#' @examples
#' check_empty_beautier_folder()
#'
#' # StrictClock
#' get_clock_model_name(create_strict_clock_model())
#'
#' # RelaxedClock
#' get_clock_model_name(create_rln_clock_model())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_clock_model_name <- function(
  clock_model
) {
  if (is_strict_clock_model(clock_model)) {
    return("StrictClock")
  } else {
    # Will fail on unimplemented clock models
    check_true(is_rln_clock_model(clock_model))
    return("RelaxedClock")
  }
}
