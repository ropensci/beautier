#' Determines if there is at least one non-strict clock model
#' in the list of one or more clock models
#' @inheritParams default_params_doc
#' @return TRUE if there is at least one non-strict clock model
#' @author Richèl J.C. Bilderbeek
#' @noRd
get_has_non_strict_clock_model <- function(clock_models) {
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  for (clock_model in clock_models) {
    if (!is_strict_clock_model(clock_model)) { # nolint beautier function
      return(TRUE)
    }
  }
  FALSE
}
