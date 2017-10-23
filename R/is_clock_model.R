#' Determine if the object is a valid clock_model
#' @param x an object, to be determined if it is a clock_model
#' @return TRUE if the clock_model is a valid clock_model, FALSE otherwise
#' @export
is_clock_model <- function(
  x
) {
  if (!"name" %in% names(x)) {
    return(FALSE)
  }
  if (!is_clock_model_name(x$name)) {
    return(FALSE)
  }
  return(TRUE)
}
