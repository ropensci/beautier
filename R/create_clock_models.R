#' Creates all supported clock models
#' @return a list of clock_models
#' @export
create_clock_models <- function() {
  return(
    list(
      create_clock_model(name = "relaxed_log_normal"),
      create_clock_model(name = "strict")
    )
  )
}
