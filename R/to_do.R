#' Indicates that something needs to be done
#' @examples
#'   x <- 4
#'   # Not  yet in the mood to respond to values of x below zero yet
#'   testit::assert(x > 0 && to_do())
to_do <- function() {
  TRUE
}
