#' Determine if the object is a valid relaxed log normal clock model
#' @param x an object, to be determined if it is a valid
#'   relaxed log normal clock model
#' @return TRUE if x is a valid relaxed log normal clock model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_rln_clock_model <- function(
  x
) {
  return("name" %in% names(x) && x$name == "relaxed_log_normal")
}
