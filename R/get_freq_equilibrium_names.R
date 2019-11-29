#' Returns valid values for the \code{freq_equilibrium} argument
#' @seealso the \code{freq_equilibrium} argument is used in
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   and \code{\link{create_tn93_site_model}}
#' @return the valid values for the \code{freq_equilibrium} argument
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' names <- beautier:::get_freq_equilibrium_names()
#' expect_true("estimated" %in% names)
#' expect_true("empirical" %in% names)
#' expect_true("all_equal" %in% names)
#' @export
get_freq_equilibrium_names <- function() {
  c("estimated", "empirical", "all_equal")
}
