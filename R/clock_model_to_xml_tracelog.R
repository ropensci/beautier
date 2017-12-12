clock_model_to_xml_tracelog <- function(
  clock_model,
  is_first
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (beautier::is_rln_clock_model(clock_model)) {
    text <- c(text, paste0("<log idref=\"ucldStdev.c:", id, "\"/>"))
    text <- c(text, paste0("<log id=\"rate.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>"))
  }
  if (is_first == FALSE) {
    text <- c(text, paste0("<log idref=\"clockRate.c:", id, "\"/>"))
  }
  text
}
