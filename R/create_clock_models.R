#' Creates all supported clock models
#' @return a list of clock_models
#' @export
create_clock_models <- function() {
  return(
    list(
      create_rln_clock_model(),
      create_strict_clock_model()
    )
  )
}
