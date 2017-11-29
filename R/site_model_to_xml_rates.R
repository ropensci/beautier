#' Converts a site model to XML,
#'   used in the \code{rates} section
#' @param site_model a site model,
#'   as created by \code{\link{create_site_model}})
#' @return the site model as XML text
#' @author Richel J.C. Bilderbeek
site_model_to_xml_rates <- function(
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))
  rates <- NULL
  if (is_gtr_site_model(site_model)) {
    rates <- c(rates, paste0("<parameter id=\"rateAC.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    rates <- c(rates, paste0("<parameter id=\"rateAG.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    rates <- c(rates, paste0("<parameter id=\"rateAT.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    rates <- c(rates, paste0("<parameter id=\"rateCG.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    rates <- c(rates, paste0("<parameter id=\"rateGT.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
  } else if (is_hky_site_model(site_model)) {
    rates <- c(rates, paste0("<parameter id=\"kappa.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">",
      beautier::get_kappa(site_model), "</parameter>"))
  } else if (is_tn93_site_model(site_model)) {
    rates <- c(rates, paste0("<parameter id=\"kappa1.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
    rates <- c(rates, paste0("<parameter id=\"kappa2.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
  }
  rates <- beautier::indent(rates, n_spaces = 4)
  # There are three parts:
  # 1) rates
  # 2) freq
  # 3) gamma shape
  # Order is determined by site model and Gamma Category Count :-(
  freqparams <- NULL
  if (!is_jc69_site_model(site_model)) {
    freqparams <- beautier::indent(
      paste0(
        "<parameter ",
        "id=\"freqParameter.s:", id, "\" dimension=\"4\" lower=\"0.0\" ",
        "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
      ),
      n_spaces = 4
    )
  }
  gamma_shape <- beautier::indent(
    paste0(
      "<parameter ",
      "id=\"gammaShape.s:", id, "\" ",
      "name=\"stateNode\">",
      beautier::get_gamma_shape(get_gamma_site_model(site_model)),
      "</parameter>"
    ),
    n_spaces = 4
  )
  gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_model)) # nolint
  prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_model)) # nolint
  text <- NULL
  if (gcc == 0) {
    text <- c(text, rates)
    text <- c(text, freqparams)
  } else if (gcc == 1) {
    if (is_gtr_site_model(site_model)) {
      text <- c(text, freqparams)
      text <- c(text, rates)
    } else {
      text <- c(text, rates)
      text <- c(text, freqparams)
    }
  } else {
    if (is_gtr_site_model(site_model)) {
      if (prop_invariant == get_default_prop_invariant()) {
        text <- c(text, freqparams)
        text <- c(text, rates)
        text <- c(text, gamma_shape)
      } else {
        text <- c(text, gamma_shape)
        text <- c(text, freqparams)
        text <- c(text, rates)
      }
    } else {
      text <- c(text, rates)
      text <- c(text, gamma_shape)
      text <- c(text, freqparams)
    }
  }
  text
}
