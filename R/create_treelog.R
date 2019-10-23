#' Create a \code{treelog} object
#' @param filename name of the file to store the posterior trees
#' @param log_every number of MCMC states between the logging of that state
#' phylogenies to. By default, this is \code{$(tree).trees}
#' @param mode mode how to log.
#' Valid are \code{tree}, \code{autodetect} and \code{compound}
#' @export
create_treelog <- function(
  filename = "$(tree).trees",
  log_every = 1000,
  mode = "tree"
) {
  treelog <- list(
    filename = filename,
    log_every = log_every,
    mode = mode
  )
  beautier::check_treelog(treelog)
  treelog
}
