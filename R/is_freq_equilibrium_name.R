#' Checks if \code{name} is a valid \code{freq_equilibrium} argument value
#' @param name the name to check if it is a valid \code{freq_equilibrium}
#'   argument value
#' @seealso the \code{freq_equilibrium} argument is used by
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   and \code{\link{create_tn93_site_model}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testit::assert(beautier:::is_freq_equilibrium_name("estimated"))
#'   testit::assert(beautier:::is_freq_equilibrium_name("empirical"))
#'   testit::assert(beautier:::is_freq_equilibrium_name("all_equal"))
is_freq_equilibrium_name <- function(
  name
) {
  name %in% get_freq_equilibrium_names()
}
