#' Is it needed to add the tip dates as a string to the BEAST2 XML input file?
#'
#' The hypothesis is that this is needed when
#' all times are zero.
#' @inheritParams default_params_doc
#' @return TRUE if this is needed
#' @author Richèl J.C. Bilderbeek
#' @export
needs_trait_set_str <- function(inference_model) {
  check_inference_model(inference_model)

  if (is_one_na(inference_model$tipdates_filename)) {
    return(FALSE)
  }
  t <- read_tipdates_file(inference_model$tipdates_filename)
  !all(t$time == "0")
}
