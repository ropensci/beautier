#' Determine if the object is a valid
#' log-normal distribution
#' @param x an object, to be determined if it is a valid
#'   log-normal distribution
#' @return TRUE if x is a valid log-normal distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_log_normal_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "log_normal")
}
