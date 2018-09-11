#' Determine if the MCMC is a default MCMC
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_true(beautier:::is_default_mcmc(create_mcmc()))
#' @noRd
is_default_mcmc <- function(mcmc) {
  if (!is_mcmc(mcmc)) return(FALSE)
  length(names(mcmc)) == 2
}
