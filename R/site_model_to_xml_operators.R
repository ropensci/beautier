#' Converts a site model to XML,
#'   used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
site_model_to_xml_operators <- function(
  site_model
) {
  testit::assert(is_site_model(site_model)) # nolint beautier function

  # May be NA for JC69 model
  id <- site_model$id

  text <- NULL

  if (is_hky_site_model(site_model)) { # nolint beautier function
    testit::assert(is_id(id)) # nolint beautier function
    text <- c(text, paste0("<operator id=\"KappaScaler.s:", id, "\" ",
      "spec=\"ScaleOperator\" parameter=\"@kappa.s:", id, "\" ",
      "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
  } else if (is_tn93_site_model(site_model)) { # nolint beautier function
    testit::assert(is_id(id)) # nolint beautier function
    if (site_model$kappa_1_param$estimate == TRUE) {
      text <- c(text, paste0("<operator id=\"kappa1Scaler.s:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@kappa1.s:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
    }
    if (site_model$kappa_2_param$estimate == TRUE) {
      text <- c(text, paste0("<operator id=\"kappa2Scaler.s:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@kappa2.s:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
    }
  } else if (is_gtr_site_model(site_model)) { # nolint beautier function
    testit::assert(is_id(id)) # nolint beautier function
    if (site_model$rate_ac_param$estimate == TRUE) {
      text <- c(text, paste0("<operator id=\"RateACScaler.s:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@rateAC.s:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
    }
    if (site_model$rate_ag_param$estimate == TRUE) {
      text <- c(text, paste0("<operator id=\"RateAGScaler.s:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@rateAG.s:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
    }
    if (site_model$rate_at_param$estimate == TRUE) {
      text <- c(text, paste0("<operator id=\"RateATScaler.s:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@rateAT.s:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
    }
    if (site_model$rate_cg_param$estimate == TRUE) {
      text <- c(text, paste0("<operator id=\"RateCGScaler.s:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@rateCG.s:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
    }
    if (site_model$rate_gt_param$estimate == TRUE) {
      text <- c(text, paste0("<operator id=\"RateGTScaler.s:", id, "\" ",
        "spec=\"ScaleOperator\" parameter=\"@rateGT.s:", id, "\" ",
        "scaleFactor=\"0.5\" weight=\"0.1\"/>")) # nolint this is no absolute path
    }
  }
  if (!is_jc69_site_model(site_model)) { # nolint beautier function
    testit::assert(is_id(id)) # nolint beautier function
    text <- c(text, paste0("<operator ",
      "id=\"FrequenciesExchanger.s:", id, "\" spec=\"DeltaExchangeOperator\" ",
      "delta=\"0.01\" weight=\"0.1\">"))
    text <- c(text, paste0("    <parameter ",
      "idref=\"freqParameter.s:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("</operator>"))
  }

  if (site_model$gamma_site_model$gamma_cat_count > 1) {
    testit::assert(is_id(id)) # nolint beautier function
    text <- c(text, paste0("<operator ",
      "id=\"gammaShapeScaler.s:", id, "\" spec=\"ScaleOperator\" ",
      "parameter=\"@gammaShape.s:", id, "\" scaleFactor=\"0.5\" ",
      "weight=\"0.1\"/>")) # nolint this is no absolute path
  }
  text
}
