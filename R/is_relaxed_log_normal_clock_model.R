#' Determine if the object is a valid relaxed log normal clock model
#' @param x an object, to be determined if it is a valid
#'   relaxed log normal clock model
#' @return TRUE if x is a valid relaxed log normal clock model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#'   rln_clock_model <- create_rln_clock_model()
#'   testit::assert(is_rln_clock_model(rln_clock_model))
#'
#'   strict_clock_model <- create_strict_clock_model()
#'   testit::assert(!is_rln_clock_model(strict_clock_model))
#' @export
is_rln_clock_model <- function(
  x
) {
  return("name" %in% names(x) && x$name == "relaxed_log_normal")
}
