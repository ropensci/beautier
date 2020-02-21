#' Rename the filenames within an MCMC
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
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
#' expect_equal(mcmc$tracelog$filename, "/home/john/trace.log")
#' expect_equal(mcmc$screenlog$filename, "/home/john/screen.log")
#' expect_equal(mcmc$treelog$filename, "/home/john/tree.log")
#'
#' # Nah, files should be put in '/home/doe' folder instead
#' mcmc <- rename_mcmc_filenames(
#'   mcmc = mcmc,
#'   rename_fun = get_replace_dir_fun("/home/doe")
#' )
#'
#' expect_equal(mcmc$tracelog$filename, "/home/doe/trace.log")
#' expect_equal(mcmc$screenlog$filename, "/home/doe/screen.log")
#' expect_equal(mcmc$treelog$filename, "/home/doe/tree.log")
#'
#' # Nah, files should be put in local folder instead
#' mcmc <- rename_mcmc_filenames(
#'   mcmc = mcmc,
#'   rename_fun = get_remove_dir_fun()
#' )
#'
#' expect_equal(mcmc$tracelog$filename, "trace.log")
#' expect_equal(mcmc$screenlog$filename, "screen.log")
#' expect_equal(mcmc$treelog$filename, "tree.log")
#' @export
rename_mcmc_filenames <- function(
  mcmc,
  rename_fun
) {
  beautier::check_mcmc(mcmc)
  beautier::check_rename_fun(rename_fun)
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
