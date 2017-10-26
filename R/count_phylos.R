#' Count the number of phylo objects in vector x
#' @param x a vector of phylo objects
#' @return the number of phylo objects
#' @note use \code{\link{get_nth_phylo}} to extract the nth phylo object
#' @author Richel J.C. Bilderbeek
#' @export
count_phylos <- function(
  x
) {
  if (length(x) == 0) {
    return(0)
  }
  if (any(is.na(x))) {
    stop("object must not be NA")
  }
  if (length(x) %% 4 != 0) {
    stop("object must contain of phylo objects")
  }
  return(length(x)/4)
}
