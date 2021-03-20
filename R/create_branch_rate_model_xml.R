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
  }
  else {
    paste0(
      "<branchRateModel id=\"StrictClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", id, "\"/>" # nolint this is no absolute path
    )
  }
}

#' Internal function to call \link{create_branch_rate_model_xml}
#' for a relaxed log-normal clock.
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
create_branch_rate_model_rln_xml <- function(# nolint long function name, which is fine for a long function
  inference_model
) {
  testthat::expect_true(
    beautier::is_rln_clock_model(inference_model$clock_model)
  )
  # Do not be smart yet
  clock_model <- inference_model$clock_model
  mrca_priors <- list(inference_model$mrca_prior)

  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL


  n_discrete_rates <- clock_model$n_rate_categories
  mparam_id <- clock_model$mparam_id
  line <- paste0("<branchRateModel ",
    "id=\"RelaxedClock.c:", id, "\" ",
    "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
    ifelse(!beautier::is_mrca_prior_with_distr(mrca_priors[[1]]),
      "",
      paste0("clock.rate=\"@ucldMean.c:", id, "\" ")
    ),
    ifelse(clock_model$normalize_mean_clock_rate == TRUE,
      "normalize=\"true\" ", ""),
    ifelse(n_discrete_rates != -1,
      paste0("numberOfDiscreteRates=\"", n_discrete_rates, "\" "),
      ""
    ),
    "rateCategories=\"@rateCategories.c:", id, "\" ",
    "tree=\"@Tree.t:", id, "\">"
  )

  text <- c(text, line)
  text <- c(text, paste0("    <LogNormal ",
    "id=\"LogNormalDistributionModel.c:", id, "\" ",
    "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"))
  text <- c(text, paste0("        <parameter ",
    "id=\"RealParameter.", mparam_id, "\" ",
    "estimate=\"false\" lower=\"0.0\" name=\"M\" ",
    "upper=\"1.0\">1.0</parameter>"))
  text <- c(text, paste0("    </LogNormal>"))
  if (!beautier::is_mrca_prior_with_distr(mrca_priors[[1]])) {
    text <- c(text, paste0("    <parameter ",
      "id=\"ucldMean.c:", id, "\" estimate=\"false\" ",
      "name=\"clock.rate\">", clock_model$mean_clock_rate, "</parameter>"))
  }
  text <- c(text, paste0("</branchRateModel>"))
  testit::assert(is.null(text) || beautier::is_xml(text))
  text
}
