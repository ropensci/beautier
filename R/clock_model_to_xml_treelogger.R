clock_model_to_xml_treelogger <- function(
  clock_model
) {
  testit::assert(is_clock_model(clock_model))
  id <- clock_model$id

  if (is_strict_clock_model(clock_model)) {
    return(
      paste0(
        "<log ",
        "id=\"TreeWithMetaDataLogger.t:", id, "\" ",
        "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
        "tree=\"@Tree.t:", id, "\"/>"
      )
    )
  } else {

    # Will fail on unimplemented clock models
    testit::assert(is_rln_clock_model(clock_model))

    return(
      paste0(
        "<log id=\"TreeWithMetaDataLogger.t:", id, "\" ",
        "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
        "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
        "tree=\"@Tree.t:", id, "\"/>"
      )
    )
  }
}
