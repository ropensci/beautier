#' Of each element of x, extract the \code{id}
#' @param x one or more elements with an \code{id} element
#' @return a vector of IDs
collect_ids <- function(x) {
  ids <- rep(NA, length(x))
  i <- 1
  for (e in x) {
    ids[i] <- e$id
    i <- i + 1
  }
  ids
}
