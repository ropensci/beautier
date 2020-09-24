#' Get the filenames stored in an MCMC.
#'
#' If a filename is set to an empty string, to indicate a certain log file
#' need not be created, this (non-)filename will not be returned.
#' @inheritParams default_params_doc
#' @examples
#'
#' mcmc <- create_mcmc()
#' mcmc$tracelog$filename <- "/home/john/trace.log"
#' mcmc$screenlog$filename <- "/home/john/screen.log"
#' mcmc$treelog$filename <- "/home/john/tree.log"
#'
#' # 3 filenames
#' filenames <- get_mcmc_filenames(mcmc)
#'
#' # If there is no need to write to the screenlog file ...
#' mcmc$screenlog$filename <- ""
#'
#' # 2 filenames
#' # ... one file less will be created
#' filenames <- get_mcmc_filenames(mcmc)
#' @export
get_mcmc_filenames <- function(mcmc) {
  beautier::check_mcmc(mcmc)
  filenames <- c(
    mcmc$tracelog$filename,
    mcmc$screenlog$filename,
    mcmc$treelog$filename
  )
  filenames[filenames != ""]
}
