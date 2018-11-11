#' Creates all supported clock models,
#'   which is just a list of the types returned by
#'   \code{\link{create_rln_clock_model}},
#'   and \code{\link{create_strict_clock_model}}
#' @return a list of site_models
#' @author Richel J.C. Bilderbeek
#' @examples
#'  clock_models <- beautier:::create_clock_models()
#'  testit::assert(beautier:::is_rln_clock_model(clock_models[[1]]))
#'  testit::assert(beautier:::is_strict_clock_model(clock_models[[2]]))
create_clock_models <- function() {
  return(
    list(
      create_rln_clock_model(),
      create_strict_clock_model()
    )
  )
}
