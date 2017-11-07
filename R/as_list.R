#' Create a list of the element, in which index 1 contains
#' object a, index 2 contains object b, etcetera
#' @param a desired first element of the list
#' @param b desired second element of the list
#' @return a list with object a as its first element, object b
#'   as its second element, etcetera
#' @author Richel J.C. Bilderbeek
#' @examples
#'   v <- as_list(create_hky_site_model(), create_jc69_site_model())
#'   testit::assert(is_hky_site_model(v[[1]]))
#'   testit::assert(is_jc69_site_model(v[[2]]))
#' @export
as_list <- function(a, b) {

  v <- list()
  v[[1]] <- a
  v[[2]] <- b
  v
}
