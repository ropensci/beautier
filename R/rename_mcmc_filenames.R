#' Rename the filenames within an MCMC
#' @inheritParams default_params_doc
#' @return an `mcmc` (see \link{create_mcmc}) with renamed filenames
#' @examples
#' check_empty_beautier_folder()
#'
#' # Create an MCMC with local filenames
#' mcmc <- create_mcmc()
#' mcmc$tracelog$filename <- "trace.log"
#' mcmc$screenlog$filename <- "screen.log"
#' mcmc$treelog$filename <- "tree.log"
#'
#' # Nah, files should be put in '/home/john' folder
#' mcmc <- rename_mcmc_filenames(
#'   mcmc = mcmc,
#'   rename_fun = get_replace_dir_fun("/home/john")
#' )
#'
#' # Nah, files should be put in '/home/doe' folder instead
#' mcmc <- rename_mcmc_filenames(
#'   mcmc = mcmc,
#'   rename_fun = get_replace_dir_fun("/home/doe")
#' )
#'
#' # Nah, files should be put in local folder instead
#' mcmc <- rename_mcmc_filenames(
#'   mcmc = mcmc,
#'   rename_fun = get_remove_dir_fun()
#' )
#'
#' check_empty_beautier_folder()
#' @export
rename_mcmc_filenames <- function(
  mcmc,
  rename_fun
) {
  check_mcmc(mcmc)
  check_rename_fun(rename_fun)
  mcmc$tracelog$filename <- rename_fun(mcmc$tracelog$filename)
  mcmc$screenlog$filename <-
    rename_fun(
      mcmc$screenlog$filename
    )
  mcmc$treelog$filename <-
    rename_fun(
      mcmc$treelog$filename
    )
  mcmc
}
