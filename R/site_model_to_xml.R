#' Converts a site model to XML,
#'   used in the \code{rates} section
#' @param id a site model's ID
#' @param site_model a site model,
#'   as created by \code{\link{create_site_model}})
#' @return the site model as XML text
#' @author Richel J.C. Bilderbeek
#' @export
site_model_to_xml_rates <- function(
  id,
  site_model
) {
  text <- NULL
  if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("<parameter id=\"rateAC.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("<parameter id=\"rateAG.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("<parameter id=\"rateAT.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("<parameter id=\"rateCG.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("<parameter id=\"rateGT.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
  } else if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("<parameter id=\"kappa.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">",
      beautier::get_kappa(site_model), "</parameter>"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("<parameter id=\"kappa1.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
    text <- c(text, paste0("<parameter id=\"kappa2.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
  }
  text
}
