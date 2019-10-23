#' Create a \code{treelog} object
#' @param filename name of the file to store the posterior trees
#' @param log_every number of MCMC states between the logging of that state
#' phylogenies to. By default, this is \code{$(tree).trees}
#' @export
create_treelog <- function(
  filename = "$(tree).trees",
  log_every = 1000
) {
  treelog <- list(
    filename = filename,
    log_every = log_every
  )
  beautier::check_treelog(treelog)
  treelog
}
