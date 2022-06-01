#' Determine if the MCMC is a default MCMC
#' @inheritParams default_params_doc
#' @return TRUE if the MCMC is a default MCMC
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE: An MCMC created by 'create_mcmc' is default.
#' is_default_mcmc(create_mcmc())
#'
#' # FALSE: An MCMC created by 'create_ns_mcmc' is not
#' is_default_mcmc(create_ns_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_default_mcmc <- function(mcmc) {
  if (!beautier::is_mcmc(mcmc)) return(FALSE)
  length(names(mcmc)) == length(names(beautier::create_mcmc()))
}
