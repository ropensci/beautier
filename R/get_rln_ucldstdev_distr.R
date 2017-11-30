#' Extract the ucldstdev distribution a Relaxed
#' log-normal clock model
#' @param rln_clock_model a Yule tree prior, as created
#'   by \code{\link{create_rln_clock_model}}
#' @return the uclstdev distribution
#' @author Richel J.C. Bilderbeek
#' @export
get_rln_ucldstdev_distr <- function(rln_clock_model) {

  if (!is_rln_clock_model(rln_clock_model)) {
    stop("rln_clock_model must be a rln_clock_model")
  }
  testit::assert("ucldstdev_distr" %in% names(rln_clock_model))
  rln_clock_model$ucldstdev_distr
}
