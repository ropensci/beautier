#' Internal function
#'
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

  if (1 == 2) {
    # Code below must become something like this
    beautier::indent(
      beautier::distr_to_xml_log_normal(
        distr = beautier::create_log_normal_distr(
          id = id,
          m = beautier::create_m_param(id = mparam_id),
          s = beautier::create_s_param(id = id)
        )
      )
    )
  }
  text <- c(text, paste0("    <LogNormal ",
    "id=\"LogNormalDistributionModel.c:", id, "\" ",
    "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"))
  text <- c(
    text,
    indent(
      indent(
        beautier::m_param_to_xml(
          m_param = beautier::create_m_param(
            id = mparam_id,
            lower = "0.0",
            upper = "1.0",
            value = "1.0"
          )
        )
      )
    )
  )
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
