#' Determine if the input is an inference model
#' @param x object to be determined of if it is an inference model
#' @return TRUE if the object is an inference model
#' @export
is_inference_model <- function(x) {
  result <- FALSE
  tryCatch({
    check_inference_model(x) # nolint beautier function
    result <- TRUE
  },
    error = function(e) {} # nolint do not care about e
  )
  result
}
