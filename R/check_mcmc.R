#' Check if the MCMC is a valid MCMC object.
#'
#' Calls \code{stop} if the MCMC is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \code{\link{create_mcmc}} to create a valid MCMC
#' @examples
#' check_empty_beautier_folder()
#'
#' check_mcmc(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_mcmc <- function(mcmc) {

  check_mcmc_list_element_names(mcmc)
  check_mcmc_values(mcmc)
}

#' Check if the MCMC has the list elements of a valid MCMC object.
#'
#' Calls \code{stop} if an element is missing
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \code{\link{create_mcmc}} to create a valid MCMC
#' @author Richèl J.C. Bilderbeek
#' @export
check_mcmc_list_element_names <- function(mcmc) {

  list_element_names <- c(
    "chain_length", "store_every", "pre_burnin", "n_init_attempts",
    "sample_from_prior", "treelog", "screenlog", "tracelog"
  )
  for (arg_name in list_element_names) {
    if (!arg_name %in% names(mcmc)) {
      stop(
        "'", arg_name, "' must be an element of an 'mcmc'. \n",
        "Tip: use 'create_mcmc'"
      )
    }
  }
}

#' Check if the MCMC has the list elements with valid values
#' for being a valid MCMC object.
#'
#' Calls \code{stop} if a value is invalid
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \code{\link{create_mcmc}} to create a valid MCMC
#' @author Richèl J.C. Bilderbeek
#' @export
check_mcmc_values <- function(mcmc) {

  if (mcmc$chain_length <= 0) {
    stop(
      "'mcmc$chain_length' must be non-zero and positive. \n",
      "'Actual value: ", mcmc$chain_length
    )
  }
  check_store_every(mcmc$store_every)
  if (!is_one_na(mcmc$store_every) &&
      mcmc$store_every > mcmc$chain_length
  ) {
    stop(
      "'mcmc$store_every' must be less than 'mcmc$chain_length'. \n",
      "Actual value of 'mcmc$chain_length': ", mcmc$chain_length, "'. \n",
      "Actual value of 'mcmc$store_every': ", mcmc$store_every, "'"
    )
  }
  if (!is_one_na(mcmc$store_every) &&
      mcmc$store_every != -1 && mcmc$store_every < 1000
  ) {
    stop("'mcmc$store_every' must be at least 1000, NA or -1")
  }

  if (mcmc$chain_length < mcmc$pre_burnin) {
    stop(
      "'mcmc$pre_burnin' must be less than 'mcmc$chain_length'",
      "Actual value 'mcmc$pre_burnin': '", mcmc$pre_burnin, "'. \n",
      "Actual value 'mcmc$chain_length': '", mcmc$chain_length, "'"
    )
  }
  lapply(mcmc$n_init_attempts, function(x) check_number_whole(x, min = 1, arg = "n_init_attempts"))
  check_logical(mcmc$sample_from_prior)
  check_treelog(mcmc$treelog)
  check_screenlog(mcmc$screenlog)
  check_tracelog(mcmc$tracelog)
  invisible(mcmc)
}
