#' Determine if the object is a valid
#' log-normal distribution,
#' as created by \code{\link{create_log_normal_distr}}
#' @param x an object, to be determined if it is a valid
#'   log-normal distribution
#' @return TRUE if x is a valid log-normal distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distribution}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @export
is_log_normal_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "log_normal")
}
