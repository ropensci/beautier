#' Converts a clock model to the \code{operators} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
clock_model_to_xml_operators <- function(
  clock_model,
  mrca_priors,
  tipdates_filename = NA
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id

  # May not need ID at all, if it is the first and strict clock model
  text <- NULL
  if (!beautier::is_strict_clock_model(clock_model)) {
    # Will fail on unimplemented clock models
    testit::assert(beautier::is_rln_clock_model(clock_model))

    testit::assert(beautier::is_id(id))
    text <- c(text, paste0("<operator id=\"ucldStdevScaler.c:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"3.0\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<operator ",
      "id=\"CategoriesRandomWalk.c:", id, "\" spec=\"IntRandomWalkOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\" ",
      "windowSize=\"1\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<operator ",
      "id=\"CategoriesSwapOperator.c:", id, "\" spec=\"SwapOperator\" ",
      "intparameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<operator ",
      "id=\"CategoriesUniform.c:", id, "\" spec=\"UniformOperator\" ",
      "parameter=\"@rateCategories.c:", id, "\" weight=\"10.0\"/>") # nolint this is no absolute path
    )
    if (beautier::is_mrca_prior_with_distr(mrca_priors[[1]])) {
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
        paste0("<operator id=\"relaxedUpDownOperator.c:", id, "\" ",
          "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"))
      text <- c(text, paste0("    <up idref=\"ucldMean.c:", id, "\"/>")) # nolint this is no absolute path
      text <- c(text, paste0("    <down idref=\"Tree.t:", id, "\"/>")) # nolint this is no absolute path
      text <- c(text, paste0("</operator>"))
    }
  }

  if ((beautier::is_strict_clock_model(clock_model)
    && beautier::is_mrca_prior_with_distr(mrca_priors[[1]])) ||
      !beautier::is_one_na(tipdates_filename)
  ) {
    text <- c(
      text,
      paste0(
        "<operator id=\"StrictClockRateScaler.c:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@clockRate.c:", id, "\" ",
        "scaleFactor=\"0.75\" weight=\"3.0\"/>" # nolint this is no absolute path
      )
    )
    text <- c(
      text,
      paste0(
        "<operator id=\"strictClockUpDownOperator.c:", id, "\" ",
        "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"
      )
    )
    text <- c(text, paste0("    <up idref=\"clockRate.c:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("    <down idref=\"Tree.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("</operator>"))
  }
  text
}
