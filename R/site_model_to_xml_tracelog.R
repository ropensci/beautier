#' Creates the site model's XML for the tracelog section
#' @inheritParams default_params_doc
#' @seealso all site models' tracelog section is created
#'   by \code{\link{site_models_to_xml_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
site_model_to_xml_tracelog <- function(
  site_model
) {
  testit::assert(is_site_model(site_model))
  id <- site_model$id

  text <- NULL
  if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("<log idref=\"kappa.s:", id, "\"/>"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("<log idref=\"kappa1.s:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"kappa2.s:", id, "\"/>"))
  } else if (is_gtr_site_model(site_model)) {
    if (site_model$rate_ac_param$estimate == TRUE) {
      text <- c(text, paste0("<log idref=\"rateAC.s:", id, "\"/>"))
    }
    if (site_model$rate_ag_param$estimate == TRUE) {
      text <- c(text, paste0("<log idref=\"rateAG.s:", id, "\"/>"))
    }
    if (site_model$rate_at_param$estimate == TRUE) {
      text <- c(text, paste0("<log idref=\"rateAT.s:", id, "\"/>"))
    }
    if (site_model$rate_cg_param$estimate == TRUE) {
      text <- c(text, paste0("<log idref=\"rateCG.s:", id, "\"/>"))
    }
    if (site_model$rate_gt_param$estimate == TRUE) {
      text <- c(text, paste0("<log idref=\"rateGT.s:", id, "\"/>"))
    }
  }
  if (!is_jc69_site_model(site_model)) {
    text <- c(text, paste0("<log idref=\"freqParameter.s:", id, "\"/>"))
  }
  if (site_model$gamma_site_model$gamma_cat_count > 1) {
    text <- c(text, paste0("<log idref=\"gammaShape.s:", id, "\"/>"))
  }
  text
}
