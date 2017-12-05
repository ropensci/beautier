#' Converts a clock model to the \code{operators} section of the
#' XML as text
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @author Richel J.C. Bilderbeek
clock_model_to_xml_operators <- function(
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_strict_clock_model(clock_model)) {
    text <- c(text, paste0("<operator ",
      "id=\"StrictClockRateScaler.c:", id, "\" ",
      "spec=\"ScaleOperator\" ",
      "parameter=\"@clockRate.c:", id, "\" ",
      "scaleFactor=\"0.75\" weight=\"3.0\"/>"))
  } else {
    # Will fail on unimplemented clock models
    testit::assert(beautier::is_rln_clock_model(clock_model))

    text <- c(text, paste0("<operator id=\"ucldStdevScaler.c:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"3.0\"/>"))
    text <- c(text, paste0("<operator ",
      "id=\"CategoriesRandomWalk.c:", id, "\" spec=\"IntRandomWalkOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\" ",
      "windowSize=\"1\"/>"))
    text <- c(text, paste0("<operator ",
      "id=\"CategoriesSwapOperator.c:", id, "\" spec=\"SwapOperator\" ",
      "intparameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>"))
    text <- c(text, paste0("<operator ",
      "id=\"CategoriesUniform.c:", id, "\" spec=\"UniformOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>"))
  }
  text <- c(text, paste0("<operator ",
    "id=\"strictClockUpDownOperator.c:", id, "\" ",
    "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"))
  text <- c(text, paste0("    <up idref=\"clockRate.c:", id, "\"/>"))
  text <- c(text, paste0("    <down idref=\"Tree.t:", id, "\"/>"))
  text <- c(text, paste0("</operator>"))

  text
}
