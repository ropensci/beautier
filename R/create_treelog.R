#' Create a \code{treelog} object
#' @param filename name of the file to store the posterior
#' phylogenies to. By default, this is \code{$(tree).trees}
#' @export
create_treelog <- function(
  filename = "$(tree).trees"
) {
  treelog <- list(
    filename = filename
  )
  beautier::check_treelog(treelog)
  treelog
}
