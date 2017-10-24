#' Extract the rate from a clock model
#' @param clock_models one or more clock_models
#' @return the kappa
#' @export
get_clock_model_rate <- function(
  clock_models
) {
  if (!is_clock_model(clock_models)) {
    stop("clock_models must be one or more clock_models")
  }
  if ("rate" %in% names(clock_models)) {
    return(clock_models$rate)
  }
  # The default value
  return(get_default_clock_model_rate())
}

#' Get the default strict clock model rate.
#' Used in, among others, create_strict_clock_model
#' @note the value is returned as a string, to be sure the testing XMLs
#'   are recreated exactly, as rounding errors are prevented
#' @export
get_default_clock_model_rate <- function() {
  return("1.0")
}
