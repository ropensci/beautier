#' Internal function to create the \code{branchRateModel} section
#' of the XML as text.
#'
#' Creates the \code{branchRateModel} section
#' of the XML as text.
#'
#' This function will be called only if there are no MRCA priors.
#'
#' The \code{distribution} tag (with ID equals \code{treeLikelihood})
#' has these elements:
#'
#' \preformatted{
#'   <branchRateModel[...]>
#'     [...]
#'   </branchRateModel>
#' }
#'
#' When there is a strict clock,
#'   \link{create_branch_rate_model_sc_xml} is called.
#' When there is an RLN clock,
#'   \link{create_branch_rate_model_rln_xml} is called.
#'
#' Zooming out:
#'
#' \preformatted{
#'   <beast[...]>
#'     <run[...]>
#'       <distribution id="posterior"[...]>
#'         <distribution id="likelihood"[...]>
#'           <distribution id="treeLikelihood"[...]>
#'              [...]
#'
#'              [this section]
#'           </distribution>
#'         </distribution>
#'       </distribution>
#'     </run>
#'   </beast>
#' }
#'
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
create_branch_rate_model_xml <- function(# nolint long function name, which is fine for a long function
  inference_model
) {
  has_no_mrca_prior <- beautier::is_one_na(inference_model$mrca_prior)
  has_non_strict_clock <- beautier::get_has_non_strict_clock_model(
    list(inference_model$clock_model)
  )
  has_mrca_prior <- !has_no_mrca_prior
  has_strict_clock <- !has_non_strict_clock

  if (has_no_mrca_prior || has_non_strict_clock) {
    if (beautier::is_strict_clock_model(inference_model$clock_model)) {
      beautier::create_branch_rate_model_sc_xml(inference_model)
    } else {
      # If this assumption fails,
      # probably a new clock model must be aded here :-)
      testthat::expect_true(
        beautier::is_rln_clock_model(inference_model$clock_model)
      )
      beautier::create_branch_rate_model_rln_xml(inference_model)
    }
  } else {
    testthat::expect_true(!(has_no_mrca_prior || has_non_strict_clock))
    testthat::expect_true(!has_no_mrca_prior && !has_non_strict_clock)
    testthat::expect_true(has_mrca_prior && has_strict_clock)
    beautier::create_branch_rate_model_stuff_xml(
      inference_model = inference_model
    )
  }
}

#' Internal function to call \link{create_branch_rate_model_xml}
#' for a strict clock.
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
create_branch_rate_model_sc_xml <- function(# nolint long function name, which is fine for a long function
  inference_model
) {
  testthat::expect_true(
    beautier::is_strict_clock_model(inference_model$clock_model)
  )

  id <- inference_model$clock_model$id

  if (beautier::is_one_na(inference_model$tipdates_filename)) {
    xml_begin <- paste0("<branchRateModel id=\"StrictClock.c:",
      id, "\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">"
    )
    # initialization may happen here
    inference_model$clock_model$clock_rate_param$id <- id
    xml_param <- beautier::parameter_to_xml(
      parameter = inference_model$clock_model$clock_rate_param,
      beauti_options = inference_model$beauti_options
    )
    xml_end <- "</branchRateModel>"

    # Layout
    c(xml_begin, beautier::indent(xml_param), xml_end)
  } else {
    paste0(
      "<branchRateModel id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", id, "\"/>" # nolint this is no absolute path
    )
  }
}
