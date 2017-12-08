#' Converts a site model to XML,
#'   used in the \code{substModel} section
#' @inheritParams default_params_doc
#' @return the site model as XML text
#' @author Richel J.C. Bilderbeek
site_model_to_xml_subst_model <- function(
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  if (beautier::is_jc69_site_model(site_model)) {
    return(
      paste0("<substModel ", "id=\"JC69.s:", id, "\" spec=\"JukesCantor\"/>")
    )
  }

  text <- NULL
  freq_equilibrium_text <- indent(
    freq_equilibrium_to_xml(site_model$freq_equilibrium, id),
    n_spaces = 4
  )

  if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"hky.s:", id, "\" spec=\"HKY\" kappa=\"@kappa.s:", id, "\">"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"tn93.s:", id, "\" spec=\"TN93\" kappa1=\"@kappa1.s:", id, "\" ",
      "kappa2=\"@kappa2.s:", id, "\">"))
  } else if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"gtr.s:", id, "\" spec=\"GTR\" rateAC=\"@rateAC.s:", id, "\" ",
      "rateAG=\"@rateAG.s:", id, "\" rateAT=\"@rateAT.s:", id, "\" ",
      "rateCG=\"@rateCG.s:", id, "\" rateGT=\"@rateGT.s:", id, "\">"))
    testit::assert(site_model$rate_ac_param$estimate == TRUE && to_do()) # nolint internal function
    testit::assert(site_model$rate_ag_param$estimate == TRUE && to_do()) # nolint internal function
    testit::assert(site_model$rate_at_param$estimate == TRUE && to_do()) # nolint internal function
    testit::assert(site_model$rate_cg_param$estimate == TRUE && to_do()) # nolint internal function
    if (site_model$rate_ct_param$estimate == FALSE) {
      site_model$rate_ct_param$id <- id
      text <- c(
        text,
        indent(
          parameter_to_xml_rate_ct(
            site_model$rate_ct_param, which_name = "rate_name"
          ), n_spaces = 4
        )
      )
    }
    testit::assert(site_model$rate_gt_param$estimate == TRUE && to_do()) # nolint internal function
  }
  text <- c(text, freq_equilibrium_text)
  text <- c(text, paste0("</substModel>"))
  text
}
