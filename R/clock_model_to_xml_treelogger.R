#' Convert a clock model to the XML of the TreeLogger
#' @inherit default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @noRd
clock_model_to_xml_treelogger <- function(
  clock_model
) {
  testit::assert(is_clock_model(clock_model)) # nolint beautier function
  id <- clock_model$id

  if (is_strict_clock_model(clock_model)) { # nolint beautier function
    return(
      paste0(
        "<log ",
        "id=\"TreeWithMetaDataLogger.t:", id, "\" ",
        "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
        "tree=\"@Tree.t:", id, "\"/>" # nolint this is no absolute path
      )
    )
  } else {

    # Will fail on unimplemented clock models
    testit::assert(is_rln_clock_model(clock_model)) # nolint beautier function

    return(
      paste0(
        "<log id=\"TreeWithMetaDataLogger.t:", id, "\" ",
        "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
        "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
        "tree=\"@Tree.t:", id, "\"/>" # nolint this is no absolute path
      )
    )
  }
}
