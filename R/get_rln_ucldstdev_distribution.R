#' Extract the Yule birth rate distribution a Yule tree prior
#' @param rln_clock_model a Yule tree prior, as created
#'   by \code{\link{create_rln_clock_model}}
#' @return the uclstdev distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_rln_ucldstdev_distribution <- function(rln_clock_model) {

  if (!is_rln_clock_model(rln_clock_model)) {
    stop("rln_clock_model must be a rln_clock_model")
  }
  testit::assert("uclstdev_distribution" %in% names(rln_clock_model))
  rln_clock_model$uclstdev_distribution
}
