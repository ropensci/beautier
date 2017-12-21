#' Returns valid values for the \code{freq_equilibrium} argument
#' @seealso the \code{freq_equilibrium} argument is used in
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   and \code{\link{create_tn93_site_model}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   names <- beautier:::get_freq_equilibrium_names()
#'   testit::assert("estimated" %in% names)
#'   testit::assert("empirical" %in% names)
#'   testit::assert("all_equal" %in% names)
get_freq_equilibrium_names <- function() {
  c("estimated", "empirical", "all_equal")
}
