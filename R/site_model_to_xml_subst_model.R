#' Converts a site model to XML,
#'   used in the \code{substModel} section
#' @param site_model a site model,
#'   as created by \code{\link{create_site_model}})
#' @return the site model as XML text
#' @author Richel J.C. Bilderbeek
site_model_to_xml_subst_model <- function(
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (beautier::is_jc69_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"JC69.s:", id, "\" spec=\"JukesCantor\"/>"))
  } else if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"hky.s:", id, "\" spec=\"HKY\" kappa=\"@kappa.s:", id, "\">"))
    text <- c(text, paste0("    <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("</substModel>"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"tn93.s:", id, "\" spec=\"TN93\" kappa1=\"@kappa1.s:", id, "\" ",
      "kappa2=\"@kappa2.s:", id, "\">"))
    text <- c(text, paste0("    <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("</substModel>"))
  } else if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"gtr.s:", id, "\" spec=\"GTR\" rateAC=\"@rateAC.s:", id, "\" ",
      "rateAG=\"@rateAG.s:", id, "\" rateAT=\"@rateAT.s:", id, "\" ",
      "rateCG=\"@rateCG.s:", id, "\" rateGT=\"@rateGT.s:", id, "\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"rateCT.s:", id, "\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"rateCT\">1.0</parameter>"))
    text <- c(text, paste0("    <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("</substModel>"))
  }
  text
}
