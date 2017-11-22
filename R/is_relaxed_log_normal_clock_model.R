#' Determine if the object is a valid relaxed log normal clock model
#' @param x an object, to be determined if it is a valid
#'   relaxed log normal clock model,
#'   as created by \code{\link{create_rln_clock_model}})
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
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "relaxed_log_normal") return(FALSE)
  if (!"uclstdev_distr" %in% names(x)) return(FALSE)
  if (!"m_parameter_id" %in% names(x)) return(FALSE)
  TRUE
}
