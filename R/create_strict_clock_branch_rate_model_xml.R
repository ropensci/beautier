#' Internal function.
#'
#' Internal function to create the \code{branchRateModel} section
#' of the XML as text, for a strict clock model
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
create_strict_clock_branch_rate_model_xml <- function(# nolint long function name, which is fine for a long function
  inference_model
) {
  check_true(
    is_strict_clock_model(inference_model$clock_model)
  )

  id <- inference_model$clock_model$id

  has_tipdates_filename <- !is_one_na(
    inference_model$tipdates_filename
  )
  has_mrca_prior <- !is_one_na(inference_model$mrca_prior)
  has_mrca_prior_distr <- NA
  if (has_mrca_prior) {
    has_mrca_prior_distr <- is_one_na(
      inference_model$mrca_prior$mrca_distr
    )
  }
  do_estimate_clock_rate <- inference_model$clock_model$clock_rate_param$estimate == TRUE # nolint indeed a long line

  if (
    (!has_tipdates_filename && !has_mrca_prior && !do_estimate_clock_rate) ||
      (!has_tipdates_filename && has_mrca_prior && has_mrca_prior_distr &&
          !do_estimate_clock_rate
      )
  ) {
    xml_begin <- paste0(
      "<branchRateModel id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"
    )
    # initialization may happen here
    inference_model$clock_model$clock_rate_param$id <- id
    xml_param <- parameter_to_xml(
      parameter = inference_model$clock_model$clock_rate_param,
      beauti_options = inference_model$beauti_options
    )
    xml_end <- "</branchRateModel>"

    # Layout
    c(xml_begin, indent(xml_param), xml_end)
  } else {
    paste0(
      "<branchRateModel id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", id, "\"/>" # nolint this is no absolute path
    )
  }
}
