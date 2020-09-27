#' Checks if \code{name} is a valid \code{freq_equilibrium} argument value
#' @param name the name to check if it is a valid \code{freq_equilibrium}
#'   argument value
#' @return TRUE if the name is a valid \code{freq_equilibrium} value
#' @seealso the \code{freq_equilibrium} argument is used by
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   and \code{\link{create_tn93_site_model}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # TRUE
#' is_freq_equilibrium_name("estimated")
#' is_freq_equilibrium_name("empirical")
#' is_freq_equilibrium_name("all_equal")
#' # FALSE
#' is_freq_equilibrium_name("nonsense")
#' @export
is_freq_equilibrium_name <- function(
  name
) {
  name %in% beautier::get_freq_equilibrium_names()
}
