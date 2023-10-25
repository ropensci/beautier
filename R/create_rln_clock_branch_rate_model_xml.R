#' Internal function
#'
#' Internal function to call \link{create_branch_rate_model_xml}
#' for a relaxed log-normal clock.
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
create_rln_clock_branch_rate_model_xml <- function(# nolint long function name, which is fine for a long function
  inference_model
) {
  check_true(
    is_rln_clock_model(inference_model$clock_model)
  )
  # Do not be smart yet
  clock_model <- inference_model$clock_model

  check_true(is_clock_model(clock_model))
  id <- clock_model$id
  check_true(is_id(id))

  text <- NULL

  n_discrete_rates <- clock_model$n_rate_categories
  mparam_id <- clock_model$mparam_id
  line <- paste0("<branchRateModel ",
    "id=\"RelaxedClock.c:", id, "\" ",
    "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" "
  )

  if (has_tip_dating(inference_model)) {
    line <- paste0(line, "clock.rate=\"@ucldMean.c:", id, "\" ")
  }
  if (has_mrca_prior_with_distr(inference_model)) {
    line <- paste0(line, "clock.rate=\"@ucldMean.c:", id, "\" ")
  }
  if (clock_model$normalize_mean_clock_rate == TRUE) {
    line <- paste0(line, "normalize=\"true\" ")
  }
  if (n_discrete_rates != -1) {
    line <- paste0(line, "numberOfDiscreteRates=\"", n_discrete_rates, "\" ")
  }

  line <- paste0(
    line,
    "rateCategories=\"@rateCategories.c:", id, "\" ",
    "tree=\"@Tree.t:", id, "\">"
  )

  text <- c(text, line)

  text <- c(
    text,
    paste0(
      "    <LogNormal ",
      "id=\"LogNormalDistributionModel.c:", id, "\" ",
      "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"
    )
  )
  text <- c(
    text,
    indent(
      indent(
        m_param_to_xml(
          m_param = create_m_param(
            id = mparam_id,
            lower = "0.0",
            upper = "1.0",
            value = "1.0"
          ),
          beauti_options = inference_model$beauti_options
        )
      )
    )
  )
  text <- c(text, paste0("    </LogNormal>"))
  if (!has_mrca_prior_with_distr(inference_model) &&
      !has_tip_dating(inference_model)
  ) {
    xml_here <- clock_rate_param_to_xml(
      clock_rate_param = create_clock_rate_param(
        id = id,
        estimate = FALSE,
        value = clock_model$mean_clock_rate
      ),
      beauti_options = inference_model$beauti_options
    )
    xml_here <- stringr::str_replace(
      xml_here,
      "id=\"clockRate.c:", "id=\"ucldMean.c:"
    )
    text <- c(text, indent(xml_here))
  }
  text <- c(text, paste0("</branchRateModel>"))
  check_true(is.null(text) || is_xml(text))
  text
}
