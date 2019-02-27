#' Converts a clock model to the \code{branchRateModel} section of the
#' XML as text.
#'
#' This function will be called only if there are no MRCA priors.
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #       HERE, where the ID of the distribution is 'likelihood'
#'  #     </distribution>
#'  # </distribution>
#' @noRd
clock_model_to_xml_lh_distr <- function(
  clock_model,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(is_clock_model(clock_model)) # nolint beautier function
  id <- clock_model$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  if (is_strict_clock_model(clock_model)) { # nolint beautier function
    if (is_one_na(tipdates_filename)) { # nolint beautier function
      text <- c(text, paste0("<branchRateModel id=\"StrictClock.c:",
        id, "\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
      # initialization may happen here
      clock_model$clock_rate_param$id <- id
      text <- c(
        text,
        indent( # nolint beautier function
          parameter_to_xml(clock_model$clock_rate_param), # nolint beautier function
          n_spaces = 4
        )
      )
      text <- c(text, "</branchRateModel>")
    }
    else {
      text <- c(
        text,
        paste0(
          "<branchRateModel id=\"StrictClock.c:", id, "\" ",
          "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
          "clock.rate=\"@clockRate.c:", id, "\"/>" # nolint this is no absolute path
        )
      )
    }
  } else if (is_rln_clock_model(clock_model)) { # nolint beautier function
    n_discrete_rates <- clock_model$n_rate_categories
    mparam_id <- clock_model$mparam_id
    line <- paste0("<branchRateModel ",
      "id=\"RelaxedClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
      ifelse(!is_mrca_prior_with_distr(mrca_priors[[1]]),
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
    if (!is_mrca_prior_with_distr(mrca_priors[[1]])) {
      text <- c(text, paste0("    <parameter ",
        "id=\"ucldMean.c:", id, "\" estimate=\"false\" ",
        "name=\"clock.rate\">", clock_model$mean_clock_rate, "</parameter>"))
    }
    text <- c(text, paste0("</branchRateModel>"))
  }


  testit::assert(is.null(text) || is_xml(text)) # nolint beautier function
  text
}
