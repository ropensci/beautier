#' Determine if the input is an inference model
#' @param x object to be determined of if it is an inference model
#' @return TRUE if the object is an inference model
#' @export
is_inference_model <- function(x) {
  result <- FALSE
  tryCatch({
    beautier::check_inference_model(x)
    result <- TRUE
  },
    error = function(e) {} # nolint do not care about e
  )
  result
}
