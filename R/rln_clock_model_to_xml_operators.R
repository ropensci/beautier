#' Internal function
#'
#' Converts an RLN clock model to the \code{operators} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
rln_clock_model_to_xml_operators <- function( # nolint indeed a long internal function name
  inference_model
) {
  # Don't be smart yet
  clock_model <- inference_model$clock_model

  check_true(is_rln_clock_model(clock_model))

  id <- clock_model$id

  text <- NULL

  check_true(is_id(id))
  text <- c(
    text,
    paste0(
      "<operator id=\"ucldStdevScaler.c:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"3.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"CategoriesRandomWalk.c:", id, "\" spec=\"IntRandomWalkOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\" ",
      "windowSize=\"1\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"CategoriesSwapOperator.c:", id, "\" spec=\"SwapOperator\" ",
      "intparameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>"
    )
  )
  text <- c(
    text,
    paste0(
      "<operator ",
      "id=\"CategoriesUniform.c:", id, "\" spec=\"UniformOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>"
    )
  )
  if (has_mrca_prior_with_distr(inference_model)) {
    text <- c(
      text,
      paste0(
        "<operator id=\"ucldMeanScaler.c:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@ucldMean.c:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"1.0\"/>" # nolint this is no absolute path
      )
    )

    text <- c(
      text,
      paste0(
        "<operator id=\"relaxedUpDownOperator.c:", id, "\" ",
        "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"
      )
    )
    text <- c(text, paste0("    <up idref=\"ucldMean.c:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("    <down idref=\"Tree.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("</operator>"))
  }
  text
}
