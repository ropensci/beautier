#' Returns valid values for the \code{freq_equilibrium} argument
#' @seealso the \code{freq_equilibrium} argument is used in
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   and \code{\link{create_tn93_site_model}}
#' @return the valid values for the \code{freq_equilibrium} argument
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' get_freq_equilibrium_names()
#'
#' check_empty_beautier_folder()
#' @export
get_freq_equilibrium_names <- function() {
  c("estimated", "empirical", "all_equal")
}
