#' Converts a site model to XML,
#'   used in the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
site_model_to_xml_subst_model <- function(
  site_model
) {
  testit::assert(is_site_model(site_model)) # nolint beautier function
  id <- site_model$id
  testit::assert(is_id(id)) # nolint beautier function

  if (is_jc69_site_model(site_model)) { # nolint beautier function
    return(
      paste0("<substModel ", "id=\"JC69.s:", id, "\" spec=\"JukesCantor\"/>") # nolint this is no absolute path
    )
  }

  text <- NULL
  freq_equilibrium_text <- indent( # nolint beautier function
    freq_equilibrium_to_xml(site_model$freq_equilibrium, id), # nolint beautier function
    n_spaces = 4
  )

  if (is_hky_site_model(site_model)) { # nolint beautier function
    text <- c(text, paste0("<substModel ",
      "id=\"hky.s:", id, "\" spec=\"HKY\" kappa=\"@kappa.s:", id, "\">"))
  } else if (is_tn93_site_model(site_model)) { # nolint beautier function
    subst_model_line <- paste0(
      "<substModel id=\"tn93.s:", id, "\" spec=\"TN93\""
    )
    if (site_model$kappa_1_param$estimate == TRUE) {
      subst_model_line <- paste0(
        subst_model_line, " kappa1=\"@kappa1.s:", id, "\" "
      )
    }
    if (site_model$kappa_2_param$estimate == TRUE) {
      subst_model_line <- paste0(
        subst_model_line, "kappa2=\"@kappa2.s:", id, "\""
      )
    }
    subst_model_line <- paste0(subst_model_line, ">")
    text <- c(text, subst_model_line)
    if (site_model$kappa_1_param$estimate == FALSE) {
      text <- c(text, paste0("<parameter id=\"kappa1.s:", id, "\" ",
        "estimate=\"false\" ",
        "lower=\"", site_model$kappa_1_param$lower, "\" ",
        "name=\"kappa1\">", site_model$kappa_1_param$value, "</parameter>")
      )
    }
    if (site_model$kappa_2_param$estimate == FALSE) {
      text <- c(text, paste0("<parameter id=\"kappa2.s:", id, "\" ",
        "estimate=\"false\" ",
        "lower=\"", site_model$kappa_2_param$lower, "\" ",
        "name=\"kappa2\">", site_model$kappa_2_param$value, "</parameter>")
      )
    }
  } else if (is_gtr_site_model(site_model)) {
    subst_model_xml <- paste0(
      "<substModel ", "id=\"gtr.s:", id, "\" spec=\"GTR\""
    )
    if (site_model$rate_ac_param$estimate == TRUE) {
      subst_model_xml <- paste0(
        subst_model_xml, " rateAC=\"@rateAC.s:", id, "\""
      )
    }
    if (site_model$rate_ag_param$estimate == TRUE) {
      subst_model_xml <- paste0(
        subst_model_xml, " rateAG=\"@rateAG.s:", id, "\""
      )
    }
    if (site_model$rate_at_param$estimate == TRUE) {
      subst_model_xml <- paste0(
        subst_model_xml, " rateAT=\"@rateAT.s:", id, "\""
      )
    }
    if (site_model$rate_cg_param$estimate == TRUE) {
      subst_model_xml <- paste0(
        subst_model_xml, " rateCG=\"@rateCG.s:", id, "\""
      )
    }
    if (site_model$rate_gt_param$estimate == TRUE) {
      subst_model_xml <- paste0(
        subst_model_xml, " rateGT=\"@rateGT.s:", id, "\""
      )
    }
    subst_model_xml <- paste0(subst_model_xml, ">")
    text <- c(text, subst_model_xml)

    if (site_model$rate_ac_param$estimate == FALSE) {
      site_model$rate_ac_param$id <- id
      text <- c(
        text,
        indent( # nolint beautier function
          parameter_to_xml_rate_ac( # nolint beautier function
            site_model$rate_ac_param, which_name = "rate_name"
          ), n_spaces = 4
        )
      )
    }


    if (site_model$rate_ag_param$estimate == FALSE) {
      site_model$rate_ag_param$id <- id
      text <- c(
        text,
        indent( # nolint beautier function
          parameter_to_xml_rate_ag( # nolint beautier function
            site_model$rate_ag_param, which_name = "rate_name"
          ), n_spaces = 4
        )
      )
    }


    if (site_model$rate_at_param$estimate == FALSE) {
      site_model$rate_at_param$id <- id
      text <- c(
        text,
        indent( # nolint beautier function
          parameter_to_xml_rate_at( # nolint beautier function
            site_model$rate_at_param, which_name = "rate_name"
          ), n_spaces = 4
        )
      )
    }


    if (site_model$rate_cg_param$estimate == FALSE) {
      site_model$rate_cg_param$id <- id
      text <- c(
        text,
        indent( # nolint beautier function
          parameter_to_xml_rate_cg( # nolint beautier function
            site_model$rate_cg_param, which_name = "rate_name"
          ), n_spaces = 4
        )
      )
    }

    if (site_model$rate_ct_param$estimate == FALSE) {
      site_model$rate_ct_param$id <- id
      text <- c(
        text,
        indent( # nolint beautier function
          parameter_to_xml_rate_ct( # nolint beautier function
            site_model$rate_ct_param, which_name = "rate_name"
          ), n_spaces = 4
        )
      )
    }
    if (site_model$rate_gt_param$estimate == FALSE) {
      site_model$rate_gt_param$id <- id
      text <- c(
        text,
        indent( # nolint beautier function
          parameter_to_xml_rate_gt( # nolint beautier function
            site_model$rate_gt_param,
            which_name = "rate_name"
          ), n_spaces = 4
        )
      )
    }
  }
  text <- c(text, freq_equilibrium_text)
  text <- c(text, paste0("</substModel>"))
  text
}
