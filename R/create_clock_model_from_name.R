#' Create a clock model from name
#' @inheritParams default_params_doc
#' @return a clock model,
#'   as can be created by \link{create_clock_model}
#' @seealso Use \code{\link{create_clock_model}} to create a clock model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' create_clock_model_from_name(get_clock_model_names()[1])
#' @export
create_clock_model_from_name <- function(clock_model_name) {
  if (clock_model_name == "strict") {
    beautier::create_strict_clock_model()
  } else {
    testit::assert(clock_model_name == "relaxed_log_normal")
    beautier::create_rln_clock_model()
  }
}
